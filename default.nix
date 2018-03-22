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
    displayName = "Reflex ToDo";
    resources = ./static/res;
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "samtay.reflex.todo";
    bundleName = "Reflex ToDo";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  tools = ghc: with ghc; [
    ghcid
  ];
})
