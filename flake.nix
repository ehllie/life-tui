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
            inherit (prev) installShellFiles;
            pkg = developPackage { root = ./.; };
          in
          overrideCabal
            (old: {
              buildTools = (old.buildTools or [ ]) ++ [ installShellFiles ];

              preBuild = ''
                export LIFE_TEMPLATE_DIR=$out/share/life-tui/templates
                mkdir -p $LIFE_TEMPLATE_DIR
                cp -r $src/templates/* $LIFE_TEMPLATE_DIR
              '';

              postInstall = ''
                installShellCompletion --cmd life-tui \
                 --bash <($out/bin/life-tui --bash-completion-script $out/bin/life-tui) \
                 --fish <($out/bin/life-tui --fish-completion-script $out/bin/life-tui) \
                 --zsh <($out/bin/life-tui --zsh-completion-script $out/bin/life-tui) \
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
