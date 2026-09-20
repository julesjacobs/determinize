"""Small exact Storm experiments; independent of the repository's Lean exporter."""
from fractions import Fraction as F
import stormpy


def matrix(size, entries):
    b = stormpy.ExactSparseMatrixBuilder(rows=size, columns=size, entries=len(entries))
    for i, j, x in sorted(entries):
        b.add_next_value(i, j, stormpy.Rational(str(x)))
    return b.build()


def query(edges, targets, rewards=None, transition_rewards=None, probability=False):
    n = 1 + max(max(i, j) for i, j, _ in edges)
    labels = stormpy.StateLabeling(n)
    labels.add_label('done')
    for i in targets:
        labels.add_label_to_state('done', i)
    kwargs = {}
    if rewards is not None:
        kwargs['optional_state_reward_vector'] = [stormpy.Rational(str(x)) for x in rewards]
    if transition_rewards is not None:
        kwargs['optional_transition_reward_matrix'] = matrix(n, transition_rewards)
    model = stormpy.SparseExactDtmc(stormpy.SparseExactModelComponents(
        matrix(n, edges), labels, {} if probability else {'': stormpy.SparseExactRewardModel(**kwargs)}))
    prop, = stormpy.parse_properties(('P' if probability else 'R') + '=? [ F "done" ]')
    result = stormpy.model_checking(model, prop, only_initial_states=False)
    return [F(str(x)) for x in result.get_values()]


edges = [(0, 0, F(1, 2)), (0, 1, F(1, 2)), (1, 1, F(1))]
first = query(edges, {1}, transition_rewards=[(0, 0, F(1))])
assert first[0] == 1
# E[(r + X_next)^2] = r^2 + 2*r*E[X_next] + E[X_next^2].
second = query(edges, {1}, rewards=[F(1, 2) * (1 + 2 * first[0]), F(0)])
assert second[0] == 3
print(f'geometric: mean={first[0]}, second={second[0]}, variance={second[0]-first[0]**2}')

# Continue with reward 1 (1/2), return 0 (1/4), or reject (1/4).
edges = [(0, 0, F(1, 2)), (0, 1, F(1, 4)), (0, 2, F(1, 4)),
         (1, 1, F(1)), (2, 2, F(1))]
h = query(edges, {1}, probability=True)
naive = query(edges, {1, 2}, transition_rewards=[(0, 0, F(1))])
first = query(edges, {1, 2}, rewards=[F(1, 2) * h[0], F(0), F(0)])
second = query(edges, {1, 2}, rewards=[F(1, 2) * (h[0] + 2 * first[0]), F(0), F(0)])
assert (h[0], naive[0], first[0], second[0]) == (F(1,2), F(1), F(1,2), F(3,2))
print(f'rejection: mass={h[0]}, naive reward={naive[0]}, first={first[0]}, second={second[0]}')

# Two outcomes have the same successor but different rewards.
# Averaging rewards preserves the first moment but loses the second.
edges = [(0, 1, F(1)), (1, 1, F(1))]
first = query(edges, {1}, rewards=[F(1), F(0)])
second = query(edges, {1}, rewards=[F(2), F(0)])
assert (first[0], second[0]) == (F(1), F(2))
print(f'parallel outcomes (reward 0 or 2): first={first[0]}, second={second[0]}, square of mean reward={first[0]**2}')

# Affine recursion: return 1 with probability 1/2, else return (3/2)*f().
# First moment equation is m = 1/2 + (3/4)*m.
affine = query([(0, 0, F(3,4)), (0, 1, F(1,4)), (1, 1, F(1))],
               {1}, rewards=[F(1,2), F(0)])
assert affine[0] == 2
print(f'affine geometric factor 3/2: first={affine[0]}; second diverges since (1/2)*(3/2)^2 > 1')

# A nonnegative weighted kernel can have row sums above one but spectral radius < 1.
# T = [[0,2],[1/10,0]], w=(15/4,11/8), and T*w < w componentwise.
w = [F(15,4), F(11,8)]
weighted = [[F(0), F(2)], [F(1,10), F(0)]]
assert all(sum(weighted[i][j]*w[j] for j in range(2)) < w[i] for i in range(2))
scaled_edges = []
for i in range(2):
    row = [(i, j, weighted[i][j]*w[j]/w[i]) for j in range(2) if weighted[i][j]]
    scaled_edges.extend(row + [(i, 2, 1-sum(p for _, _, p in row))])
scaled_edges.append((2,2,F(1)))
u = query(scaled_edges, {2}, rewards=[1/w[0], F(0), F(0)])
m = [u[i]*w[i] for i in range(2)]
assert m == [F(5,4),F(1,8)]
print(f'weighted affine equations via scaled Storm DTMC: values={m}')

# Signed coefficient -3/2: the weighted continuation toggles the sign state.
sign_edges = [(0,1,F(3,4)), (0,2,F(1,4)),
              (1,0,F(3,4)), (1,2,F(1,4)), (2,2,F(1))]
pos = query(sign_edges, {2}, rewards=[F(1,2),F(0),F(0)])
neg = query(sign_edges, {2}, rewards=[F(0),F(1,2),F(0)])
assert pos[0]-neg[0] == F(2,7)
print(f'signed affine factor -3/2: first={pos[0]-neg[0]}')

# Independently check Pro's proposed determinized packing targets.
controls = [(0,0)] + [(n,d) for n in range(1,10) for d in (-1,0,1)]
indices = {s:i for i,s in enumerate(controls)}
sink = len(controls)
rows = []
for n,d in controls:
    outcomes = []
    for p,cost,accepted,new_d in [(F(1,2),F(1),True,d),
            (F(1,4),F(23,20),d<=0,d+1), (F(1,4),F(17,20),d>=0,d-1)]:
        target = (n+1,new_d) if accepted else (n,d)
        j = sink if target[0] == 10 else indices[target]
        outcomes.append((p,cost,j))
    rows.append(outcomes)
packing_edges = []
for i,row in enumerate(rows):
    combined = {}
    for p,_,j in row:
        combined[j] = combined.get(j,F(0)) + p
    packing_edges.extend((i,j,p) for j,p in combined.items())
packing_edges.append((sink,sink,F(1)))
packing_first = query(packing_edges,{sink},rewards=[sum(p*r for p,r,_ in row) for row in rows]+[F(0)])
packing_second = query(packing_edges,{sink},rewards=[
    sum(p*(r*r+2*r*packing_first[j]) for p,r,j in row) for row in rows]+[F(0)])
assert packing_first[0] == F(118513705,10077696)
assert packing_second[0] == F(227517128669,1612431360)
print(f'packing: {sink} transient states; first={packing_first[0]}, second={packing_second[0]}')
