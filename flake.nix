{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlays.default = _: prev: {
        life-tui =
          let
            inherit (prev.haskellPackages) developPackage;
            inherit (prev.haskell.lib.compose) overrideCabal;
            pkg = developPackage { root = ./.; };
          in
          overrideCabal
            (drv: {
              preBuild = ''
                export LIFE_TEMPLATE_DIR=$out/lib/templates
                mkdir -p $LIFE_TEMPLATE_DIR
                cp -r $src/templates/* $LIFE_TEMPLATE_DIR
              '';
            })
            pkg;
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
