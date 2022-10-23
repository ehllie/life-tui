{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlays.default = _: prev: {
        e-gol = with prev.haskellPackages;
          developPackage { root = ./.; };
      };

    } //
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        inherit (pkgs) e-gol;
      in
      {
        packages = {
          inherit e-gol;
          default = e-gol;
        };
        devShells.default = e-gol.env;
      }));
}
