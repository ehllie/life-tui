{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlays.default = _: prev: {
        life-tui = with prev.haskellPackages;
          developPackage { root = ./.; };
      };

    } //
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        inherit (pkgs) life-tui;
      in
      {
        packages = {
          inherit life-tui;
          default = life-tui;
        };
        devShells.default = life-tui.env;
      }));
}
