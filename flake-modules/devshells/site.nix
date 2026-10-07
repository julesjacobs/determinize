# Toolchain for the site's checks (check.sh site): the sim shell, whose sim/node_modules holds
# Playwright, axe and html-validate, plus nixpkgs' headless Chromium for Playwright and lychee for
# links. Playwright drives only the browser builds of its own version, so the shell refuses to
# start unless sim/package-lock.json pins @playwright/test to nixpkgs' playwright-driver.
{
  perSystem =
    { config, lib, pkgs, ... }:
    let
      packageLock = lib.importJSON ../../sim/package-lock.json;
      playwright = packageLock.packages."node_modules/@playwright/test".version;
      browsers = pkgs.playwright-driver.browsers.override {
        withChromium = false;
        withFirefox = false;
        withWebkit = false;
        withFfmpeg = false;
      };
    in
    {
      devShells.site =
        assert lib.assertMsg (playwright == pkgs.playwright-driver.version) ''
          sim/package-lock.json pins @playwright/test ${playwright}, but nixpkgs' browsers are for
          Playwright ${pkgs.playwright-driver.version}. In sim/, run
          npm install --save-exact @playwright/test@${pkgs.playwright-driver.version}'';
        pkgs.mkShell {
          name = "determinize-site";

          # The sim shell's shell hook, which inputsFrom adds, links sim/node_modules from npmDeps.
          inputsFrom = [ config.devShells.sim ];
          inherit (config.devShells.sim) npmDeps;

          packages = [ pkgs.lychee ];

          PLAYWRIGHT_BROWSERS_PATH = browsers;
          PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
        };
    };
}
