{
  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        package-name = "rmexpbak";
        haskellPackages = nixpkgs.legacyPackages.${system}.haskellPackages;
      in {
        packages.${package-name} = haskellPackages.callCabal2nix package-name ./. {};
        defaultPackage = self.packages.${system}.${package-name};
      }
    );
}
