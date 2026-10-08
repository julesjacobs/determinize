// The step table's run in pages. The run of a deep recursion holds millions of nodes, which would
// take the page's thread a second to receive in one message and to show, so the worker that
// computes the run keeps it and sends an overview with one page of frames at a time. A page holds
// at most `maxPageFrames` frames, and at most `maxPageSize` nodes and affine terms in their states.
import type { TracePageRequest, TraceRequest, TraceResponse } from "./protocol.ts";
import type { CoupledTrace, Frame } from "./runtime/semantics.ts";
import { stateSize } from "./runtime/semantics.ts";
import { hasDomainError, runCoupling } from "./trace.ts";

/** The most frames on a page of the step table. */
export const maxPageFrames = 200;
/** The most nodes and affine terms in the states of a page's frames: the last pages of a recursion
 * 2000 deep take a few milliseconds to send and to show. */
export const maxPageSize = 20000;

/** The step table's run without its frames: how many it has, where its pages start, and what the
 * table says about the run as a whole. */
export interface TraceOverview extends Omit<CoupledTrace, "frames"> {
  frameCount: number;
  /** The index of each page's first frame. */
  pageStarts: number[];
  /** The last frame's failure outside an operation's domain, if any. */
  domainFailure: string | null;
  /** Whether a frame reached a domain error in all three states. */
  domainError: boolean;
}

/** A page of the run: its frames, and the frame before them, against which the first one's change
 * is shown. */
export interface TracePage {
  index: number;
  first: number;
  previous: Frame | null;
  frames: Frame[];
}

function frameSize(frame: Frame) {
  return stateSize(frame.original) + stateSize(frame.symbolic) + stateSize(frame.determinized);
}

/** The index of each page's first frame. */
export function pageStarts(frames: Frame[]): number[] {
  const starts: number[] = [];
  let size = Infinity;
  let count = 0;
  for (const [index, frame] of frames.entries()) {
    const next = frameSize(frame);
    if (count === maxPageFrames || size + next > maxPageSize) {
      starts.push(index);
      size = 0;
      count = 0;
    }
    size += next;
    count += 1;
  }
  return starts.length > 0 ? starts : [0];
}

export function overviewOf(trace: CoupledTrace, starts: number[]): TraceOverview {
  const { frames, ...rest } = trace;
  return {
    ...rest,
    frameCount: frames.length,
    pageStarts: starts,
    domainFailure: frames.at(-1)?.domainFailure ?? null,
    domainError: frames.some(hasDomainError),
  };
}

export function pageOf(trace: CoupledTrace, starts: number[], index: number): TracePage | null {
  if (!Number.isSafeInteger(index) || index < 0 || index >= starts.length) return null;
  const first = starts[index];
  const end = starts[index + 1] ?? trace.frames.length;
  return {
    index,
    first,
    previous: trace.frames[first - 1] ?? null,
    frames: trace.frames.slice(first, end),
  };
}

/** The page of the frame `step`. */
export function pageIndexOf(starts: number[], step: number) {
  let index = 0;
  while (index + 1 < starts.length && starts[index + 1] <= step) index += 1;
  return index;
}

/**
 * `trace` up to its first frame that alone exceeds a page's size, which ends the run as the size
 * bound does, so that every message stays within the bound.
 */
export function withinPageSize(trace: CoupledTrace): CoupledTrace {
  const index = trace.frames.findIndex((frame) => frameSize(frame) > maxPageSize);
  if (index === -1) return trace;
  return {
    ...trace,
    frames: trace.frames.slice(0, index),
    stopped: "size",
    finalOriginal: undefined,
    finalDeterminized: undefined,
  };
}

/** Computes runs as requested and serves their pages; the worker runs it, and so does the page
 * where no worker can start. */
export function createTraceServer() {
  let current: { generation: number; trace: CoupledTrace; starts: number[] } | null = null;
  return {
    handle(request: TraceRequest | TracePageRequest): TraceResponse | null {
      const { generation } = request;
      if (request.type === "trace-page") {
        if (!current || current.generation !== generation) return null;
        const page = pageOf(current.trace, current.starts, request.page);
        return page && { type: "trace-page", generation, page };
      }
      current = null;
      let trace: CoupledTrace;
      try {
        trace = withinPageSize(runCoupling(request.source, request.seed));
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return { type: "trace-failed", generation, message };
      }
      if (trace.frames.length === 0) {
        return {
          type: "trace-failed",
          generation,
          message: "its first state is too large to show",
        };
      }
      const starts = pageStarts(trace.frames);
      current = { generation, trace, starts };
      const page = pageOf(trace, starts, 0);
      if (!page) return null;
      return { type: "trace", generation, overview: overviewOf(trace, starts), page };
    },
  };
}
