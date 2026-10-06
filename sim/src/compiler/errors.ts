export class CompileError extends Error {
  constructor(message, from = undefined, to = undefined) {
    super(message);
    this.name = "CompileError";
    this.from = from;
    this.to = to;
  }
}
