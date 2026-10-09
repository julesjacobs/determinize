// The messages between the page and its workers: the sampling worker, which runs the batches of
// runs of both programs, the step table's worker, which computes the step table's run, and the
// exact worker, which explores both programs' finite models. A request has a generation number; a
// newer request replaces an older one, and the page ignores responses of any but the newest.
import type { ExactState } from "./exact.ts";
import type { Runs } from "./statistics.ts";
import type { TraceOverview, TracePage } from "./trace-pages.ts";

/** Run `count` runs of `source` and its determinization from run `from`; run i at seed
 * `seed` + i. */
export interface RunRequest {
  type: "run";
  generation: number;
  source: string;
  seed: number;
  from: number;
  count: number;
}

/** Stop the run of `generation`. */
export interface CancelRequest {
  type: "cancel";
  generation: number;
}

export type Request = RunRequest | CancelRequest;

/** The outcomes of the runs since the previous batch, of the source and the determinized
 * program. */
export interface BatchResponse {
  type: "batch";
  generation: number;
  source: Runs;
  determinized: Runs;
}

/** The run of `generation` has ended. */
export interface DoneResponse {
  type: "done";
  generation: number;
}

export type Response = BatchResponse | DoneResponse;

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

export function isRequest(value: unknown): value is Request {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "cancel") return true;
  return (
    value.type === "run" &&
    typeof value.source === "string" &&
    [value.seed, value.from, value.count].every(Number.isSafeInteger)
  );
}

function isDomainFailure(value: unknown) {
  return (
    value === null ||
    (isRecord(value) && typeof value.run === "number" && typeof value.message === "string")
  );
}

function isRuns(value: unknown): value is Runs {
  const nullableString = (field: unknown) => field === null || typeof field === "string";
  return (
    isRecord(value) &&
    value.values instanceof Float64Array &&
    typeof value.rejected === "number" &&
    typeof value.failed === "number" &&
    typeof value.stopped === "number" &&
    typeof value.zeroFailed === "number" &&
    nullableString(value.firstFailure) &&
    isDomainFailure(value.firstDomainFailure) &&
    isDomainFailure(value.firstZeroFailure) &&
    nullableString(value.firstValue) &&
    Array.isArray(value.draws) &&
    value.draws.every(
      (draws: unknown) =>
        isRecord(draws) && typeof draws.site === "number" && draws.values instanceof Float64Array,
    )
  );
}

export function isResponse(value: unknown): value is Response {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "batch") return isRuns(value.source) && isRuns(value.determinized);
  return value.type === "done";
}

/** Compute the step table's run of `source` at `seed`. */
export interface TraceRequest {
  type: "trace";
  generation: number;
  source: string;
  seed: number;
}

/** Send page `page` of the run of the request of `generation`. */
export interface TracePageRequest {
  type: "trace-page";
  generation: number;
  page: number;
}

/** The overview of the run that the request of `generation` asked for with its first page, a
 * later page, or why the run couldn't be computed. Structured cloning keeps the frames' shared
 * subexpressions shared. */
export type TraceResponse =
  | { type: "trace"; generation: number; overview: TraceOverview; page: TracePage }
  | { type: "trace-page"; generation: number; page: TracePage }
  | { type: "trace-failed"; generation: number; message: string };

export function isTraceRequest(value: unknown): value is TraceRequest | TracePageRequest {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "trace-page") return Number.isSafeInteger(value.page);
  return (
    value.type === "trace" && typeof value.source === "string" && Number.isSafeInteger(value.seed)
  );
}

function isExpr(value: unknown) {
  return isRecord(value) && typeof value.kind === "string";
}

function isFrame(value: unknown) {
  return (
    isRecord(value) &&
    typeof value.step === "number" &&
    isExpr(value.original) &&
    isExpr(value.symbolic) &&
    isExpr(value.determinized) &&
    Array.isArray(value.sigma) &&
    typeof value.originalOk === "boolean" &&
    typeof value.determinizedOk === "boolean"
  );
}

function isOverview(value: unknown): value is TraceOverview {
  return (
    isRecord(value) &&
    typeof value.seed === "number" &&
    Number.isSafeInteger(value.frameCount) &&
    Array.isArray(value.pageStarts) &&
    value.pageStarts.every(Number.isSafeInteger) &&
    typeof value.counterexample === "boolean" &&
    typeof value.ok === "boolean" &&
    typeof value.domainError === "boolean" &&
    (value.domainFailure === null || typeof value.domainFailure === "string") &&
    (value.stopped === null || value.stopped === "steps" || value.stopped === "size") &&
    [value.finalOriginal, value.finalDeterminized].every(
      (final) => final === undefined || isExpr(final),
    )
  );
}

function isPage(value: unknown): value is TracePage {
  return (
    isRecord(value) &&
    Number.isSafeInteger(value.index) &&
    Number.isSafeInteger(value.first) &&
    (value.previous === null || isFrame(value.previous)) &&
    Array.isArray(value.frames) &&
    value.frames.every(isFrame)
  );
}

export function isTraceResponse(value: unknown): value is TraceResponse {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "trace-failed") return typeof value.message === "string";
  if (value.type === "trace-page") return isPage(value.page);
  return value.type === "trace" && isOverview(value.overview) && isPage(value.page);
}

/** Explore the finite models of `source` and its determinization, plain or in the additive
 * mode. */
export interface ExactRequest {
  type: "exact";
  generation: number;
  source: string;
  additive: boolean;
}

/** Both programs' explorations so far; `done` once both have their outcomes. */
export interface ExactResponse {
  type: "exact";
  generation: number;
  source: ExactState;
  determinized: ExactState;
  done: boolean;
}

export function isExactRequest(value: unknown): value is ExactRequest {
  return (
    isRecord(value) &&
    value.type === "exact" &&
    typeof value.generation === "number" &&
    typeof value.source === "string" &&
    typeof value.additive === "boolean"
  );
}

function isExact(value: unknown) {
  return isRecord(value) && typeof value.fraction === "string" && typeof value.value === "number";
}

function isRanges(value: unknown) {
  return (
    Array.isArray(value) &&
    value.every(
      (range: unknown) =>
        isRecord(range) && Number.isSafeInteger(range.from) && Number.isSafeInteger(range.to),
    )
  );
}

function isExactState(value: unknown): value is ExactState {
  if (!isRecord(value)) return false;
  const counts = (...keys: string[]) => keys.every((key) => Number.isSafeInteger(value[key]));
  const message = typeof value.message === "string";
  switch (value.kind) {
    case "exploring":
      return counts("discovered");
    case "solving":
      return counts("states");
    case "finite":
      return (
        counts("states") &&
        isExact(value.returnProbability) &&
        isExact(value.rejectionProbability) &&
        (value.mean === null || isExact(value.mean)) &&
        (value.variance === null || isExact(value.variance))
      );
    case "too many":
      return counts("states", "maxStates") && message;
    case "draw":
      return (
        typeof value.distribution === "string" &&
        isRanges(value.sites) &&
        counts("state") &&
        message
      );
    case "fails":
      return (
        typeof value.detail === "string" && isRanges(value.sites) && counts("state") && message
      );
    case "not a number":
      return counts("state") && message;
    case "limit":
      return (
        ["states", "edges", "stateBytes"].includes(value.limit as string) &&
        counts("discovered") &&
        message
      );
    case "unsolved":
    case "error":
      return message;
    default:
      return false;
  }
}

export function isExactResponse(value: unknown): value is ExactResponse {
  return (
    isRecord(value) &&
    value.type === "exact" &&
    typeof value.generation === "number" &&
    isExactState(value.source) &&
    isExactState(value.determinized) &&
    typeof value.done === "boolean"
  );
}
