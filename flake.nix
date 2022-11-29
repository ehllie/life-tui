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
            inherit (prev.lib) pipe;
            inherit (prev.haskell.lib.compose)
              overrideCabal generateOptparseApplicativeCompletion;
            overrides = [
              (overrideCabal (old: {
                preBuild = ''
                  export LIFE_TEMPLATE_DIR=$out/share/life-tui/templates
                  mkdir -p $LIFE_TEMPLATE_DIR
                  cp -r $src/templates/* $LIFE_TEMPLATE_DIR
                '';
              }))
              (generateOptparseApplicativeCompletion "life-tui")
            ];
            pkg = (developPackage { root = ./.; });
          in
          pipe pkg overrides;
      };

    } //
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        inherit (pkgs) life-tui;
        inherit (pkgs.haskellPackages) haskell-language-server;
      in
      {
        packages = {
          inherit life-tui;
          default = life-tui;
        };
        devShells.default = life-tui.env.overrideAttrs
          (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ haskell-language-server ];
          });
      }));
}
