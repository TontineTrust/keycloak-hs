{
  description = "keycloak-hs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ]
      (system:
        let
          pkgs = import nixpkgs {inherit system;};

          haskellPackages = pkgs.haskellPackages;
        in
        rec
        {
          packages.keycloak-hs =
            haskellPackages.callCabal2nix "keycloak-hs" ./. rec {
              # Dependency overrides go here
            };

          defaultPackage = packages.keycloak-hs;

          devShell =
            pkgs.mkShell {
              buildInputs = with haskellPackages; [
                haskell-language-server
                ghcid
                cabal-install
              ];
              inputsFrom = [
                self.defaultPackage.${system}.env
              ];
            };
        });
}
