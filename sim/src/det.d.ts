/** A program from `examples/`: esbuild's text loader, and `test/det-loader.ts` in the tests,
 * import a `.det` file as its text. */
declare module "*.det" {
  const source: string;
  export default source;
}
