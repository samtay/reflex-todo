{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {

  name = "reflex-todo";

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "samtay.reflex.todo";
    displayName = "Reflex Todo Demo";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "samtay.reflex.todo";
    bundleName = "Reflex Todo Demo";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  tools = ghc: with ghc; [
    ghcid
  ];
})
