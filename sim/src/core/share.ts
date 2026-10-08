// The state that a link to the simulator carries: the program, the seed, the example and how the
// page shows them (the symbolic state, the chart), as the fragment `#v1=` followed by the base64url of the deflate-raw-compressed
// JSON.

export interface SharedState {
  source: string;
  seed: number;
  /** The id of the example the program came from. */
  example: string;
  /** Whether the step table shows the symbolic state; a link without it leaves the viewer's
   * choice. */
  symbolic?: boolean;
  /** The chart of the distributions band, when it is the plot against a G draw. */
  view?: "against";
}

/** What a fragment holds: no state, a state, or a state that can't be restored, with why. */
export type Decoded =
  | { kind: "none" }
  | { kind: "state"; state: SharedState }
  | { kind: "error"; message: string };

/** The largest state, in bytes of JSON, that a link may carry. */
export const maxSharedBytes = 64 * 1024;

const prefix = "#v1=";

async function bytesOf(stream: ReadableStream<Uint8Array>, limit: number) {
  const chunks: Uint8Array[] = [];
  let size = 0;
  const reader = stream.getReader();
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    size += value.length;
    if (size > limit) {
      await reader.cancel();
      return null;
    }
    chunks.push(value);
  }
  const bytes = new Uint8Array(size);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.length;
  }
  return bytes;
}

function toBase64Url(bytes: Uint8Array) {
  if (typeof bytes.toBase64 === "function") {
    return bytes.toBase64({ alphabet: "base64url", omitPadding: true });
  }
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replaceAll("+", "-").replaceAll("/", "_").replace(/=+$/, "");
}

function fromBase64Url(text: string) {
  if (typeof Uint8Array.fromBase64 === "function") {
    return Uint8Array.fromBase64(text, { alphabet: "base64url" });
  }
  const binary = atob(text.replaceAll("-", "+").replaceAll("_", "/"));
  return Uint8Array.from(binary, (char) => char.charCodeAt(0));
}

/** The fragment of a link to `state`. */
export async function encodeShare(state: SharedState) {
  const { source, seed, example, symbolic, view } = state;
  const json = JSON.stringify({
    source,
    seed,
    example,
    ...(symbolic ? { symbolic } : {}),
    ...(view === "against" ? { view } : {}),
  });
  const stream = new Blob([json]).stream().pipeThrough(new CompressionStream("deflate-raw"));
  const bytes = await bytesOf(stream, Number.POSITIVE_INFINITY);
  return `${prefix}${toBase64Url(bytes ?? new Uint8Array())}`;
}

/** The state in a link's fragment `hash`, as `location.hash` gives it. */
export async function decodeShare(hash: string): Promise<Decoded> {
  if (!hash.startsWith(prefix)) return { kind: "none" };
  const unreadable = { kind: "error", message: "This link could not be read." } as const;
  let json: unknown;
  try {
    const compressed = fromBase64Url(hash.slice(prefix.length));
    const stream = new Blob([compressed])
      .stream()
      .pipeThrough(new DecompressionStream("deflate-raw"));
    const bytes = await bytesOf(stream, maxSharedBytes);
    if (!bytes) {
      return { kind: "error", message: "This link's program is larger than 64 KiB." };
    }
    json = JSON.parse(new TextDecoder().decode(bytes));
  } catch {
    return unreadable;
  }
  if (typeof json !== "object" || json === null) return unreadable;
  const { source, seed, example, symbolic, view } = json as Record<string, unknown>;
  if (typeof source !== "string" || typeof example !== "string") return unreadable;
  if (typeof seed !== "number" || !Number.isSafeInteger(seed)) return unreadable;
  if (symbolic !== undefined && typeof symbolic !== "boolean") return unreadable;
  if (view !== undefined && view !== "against") return unreadable;
  const state: SharedState = { source, seed, example };
  if (symbolic !== undefined) state.symbolic = symbolic;
  if (view !== undefined) state.view = view;
  return { kind: "state", state };
}
