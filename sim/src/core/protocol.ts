// The messages between the page and the sampling worker. A run has a generation number; a newer
// run replaces an older one, and the page ignores responses of any but the newest.
import type { CoupledTrace } from "./runtime/semantics.ts";

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

/** The samples of the runs since the previous batch; `runs` counts the runs, with or without one. */
export interface BatchResponse {
  type: "batch";
  generation: number;
  runs: number;
  original: Float64Array;
  determinized: Float64Array;
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

export function isResponse(value: unknown): value is Response {
  if (!isRecord(value) || typeof value.generation !== "number") return false;
  if (value.type === "batch") {
    return (
      typeof value.runs === "number" &&
      value.original instanceof Float64Array &&
      value.determinized instanceof Float64Array
    );
  }
  return (
    value.type === "done" &&
    (value.last === null || (isRecord(value.last) && Array.isArray(value.last.frames)))
  );
}
