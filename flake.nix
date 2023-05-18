{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      checks = {
        default = pkgs.stdenv.mkDerivation {
          name = "sha256.fut-checks";
          buildInputs = [pkgs.futhark];

          src = ./.;

          buildPhase = ''
            futhark test -c --no-terminal ./lib/github.com/alexnortung/sha256.fut/
          '';

          installPhase = ''
            mkdir -p $out
          '';
        };
      };
      devShells = {
        default = pkgs.mkShell {
          buildInputs = [pkgs.futhark];
        };
      };
    });
}
