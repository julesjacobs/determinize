// The messages between the page and the sampling worker. A run has a generation number; a newer
// run replaces an older one, and the page ignores responses of any but the newest.
import type { CoupledTrace } from "./runtime/semantics.ts";
import type { Runs } from "./statistics.ts";

/** Run `source` at each of `seeds`, in order, until a run fails. */
export interface RunRequest {
  type: "run";
  generation: number;
  source: string;
  seeds: Float64Array;
}

/** Stop the run of `generation`. */
export interface CancelRequest {
  type: "cancel";
  generation: number;
}

export type Request = RunRequest | CancelRequest;

/** The outcomes of the runs since the previous batch, of the source and the determinized program. */
export interface BatchResponse {
  type: "batch";
  generation: number;
  source: Runs;
  determinized: Runs;
}

/** The run of `generation` has ended; `last` is the step table's run at the last seed that ran. */
export interface DoneResponse {
  type: "done";
  generation: number;
  last: CoupledTrace | null;
}

export type Response = BatchResponse | DoneResponse;

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

export function isRequest(value: unknown): value is Request {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "cancel") return true;
  return (
    value.type === "run" && typeof value.source === "string" && value.seeds instanceof Float64Array
  );
}

function isRuns(value: unknown): value is Runs {
  const nullableString = (field: unknown) => field === null || typeof field === "string";
  return (
    isRecord(value) &&
    value.values instanceof Float64Array &&
    typeof value.rejected === "number" &&
    typeof value.failed === "number" &&
    nullableString(value.firstFailure) &&
    nullableString(value.firstValue)
  );
}

export function isResponse(value: unknown): value is Response {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "batch") return isRuns(value.source) && isRuns(value.determinized);
  return (
    value.type === "done" &&
    (value.last === null || (isRecord(value.last) && Array.isArray(value.last.frames)))
  );
}
