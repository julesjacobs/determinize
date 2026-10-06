export class CompileError extends Error {
  declare from: number | undefined;
  declare to: number | undefined;

  constructor(
    message: string,
    from: number | undefined = undefined,
    to: number | undefined = undefined,
  ) {
    super(message);
    this.name = "CompileError";
    this.from = from;
    this.to = to;
  }
}
