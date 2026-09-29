{
  description = "shroom";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      projectName = "shroom";
      localPackages = {
        shroom = ./shroom;
        shroom-baikai = ./shroom-baikai;
      };

      # Always keep in sync with the tested-with section in the cabal file
      supportedGhcs = [
        "ghc910"
        "ghc912"
        # "ghc914" # Uncomment as soon as nixpkgs is more advanced
      ];

    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Haskell package overrides for dependencies
          #
          # nixpkgs-unstable does not package the baikai family at all (checked via
          # `nix eval`, 2026-09-29: only "claude" of the shroom-baikai dependency set is
          # present in haskellPackages), and its own "claude" is pinned to 1.4.0, older
          # than the 1.5.0 that baikai-claude needs. Pull all five straight from Hackage.
          dependenciesOverrides = with pkgs.haskell.lib;
            composeManyExtensions [
              (hfinal: hprev: {
                claude = hfinal.callHackageDirect
                  {
                    pkg = "claude";
                    ver = "1.5.0";
                    sha256 = "sha256-Eakrz2Eoj+6DOfPnCTpLquIrXY0pXIkevF4QDWtMFUE=";
                  }
                  { };
                baikai = hfinal.callHackageDirect
                  {
                    pkg = "baikai";
                    ver = "0.7.1.0";
                    sha256 = "sha256-MbR9bddpAUGBjFgqUBVQKZ2Sf8Deh6NHr814bofZzr8=";
                  }
                  { };
                baikai-claude = hfinal.callHackageDirect
                  {
                    pkg = "baikai-claude";
                    ver = "0.7.0.0";
                    sha256 = "sha256-Q7SBoJF2i5ie1sOx7d34/HgTpCe2llZhq61OMyMOIfk=";
                  }
                  { };
                baikai-effectful = hfinal.callHackageDirect
                  {
                    pkg = "baikai-effectful";
                    ver = "0.4.0.2";
                    sha256 = "sha256-wC43iXuTdZRqI6FfUALnpNysrcLjDyO9FN8PGHbCDAA=";
                  }
                  { };
                baikai-openai = hfinal.callHackageDirect
                  {
                    pkg = "baikai-openai";
                    ver = "0.7.0.0";
                    sha256 = "sha256-L7UoAQYiY2m2yoKVAu1CM/5M3iwoBxFaNH8Z3KCSwuU=";
                  }
                  { };
              })
            ];

          haskellPackagesFor = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.override (_: {
              overrides = dependenciesOverrides;
            }))
            (genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
            // { default = pkgs.haskell.packages.ghc912; });

          # Haskell package overrides to set the definitions of the locally defined packages to the current version in this repo
          localPackagesOverrides = hfinal: hprev: with pkgs.haskell.lib;
            (mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages);

          haskellPackagesExtended = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.override (haskellPackagesPrevious: {
              overrides = composeManyExtensions [
                haskellPackagesPrevious.overrides
                localPackagesOverrides
              ];
            }))
            haskellPackagesFor;

          localPackagesFor = haskellPackages: mapAttrs (pname: _path: haskellPackages.${pname}) localPackages;
          allLocalPackagesFor = ghcVersion: haskellPackages:
            pkgs.linkFarm "${projectName}-all-for-${ghcVersion}"
              (localPackagesFor haskellPackages);
          forEachGHC = mapAttrs allLocalPackagesFor haskellPackagesExtended;
          allGHCs = pkgs.linkFarm "${projectName}-all-ghcs" forEachGHC;
        in
        {
          # "packages" doesn't allow nested sets
          legacyPackages = mapAttrs
            (ghcVersion: haskellPackages: localPackagesFor haskellPackages // {
              "${projectName}-all" = allLocalPackagesFor ghcVersion haskellPackages;
            })
            haskellPackagesExtended // {
            "${projectName}-all" = forEachGHC;
          };

          packages = {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: attrValues (localPackagesFor (haskellPackagesExtended.${ghcVersion}));
              nativeBuildInputs = [
                haskellPackages.haskell-language-server
                pkgs.nixpkgs-fmt
                pkgs.cabal-install
              ];
            })
            haskellPackagesFor;

          formatter = pkgs.nixpkgs-fmt;
        }) // {
      inherit supportedGhcs;

      # Import into a NixOS configuration as `services.ollama.shroom.enable = true;`
      # (laptop), or with `auth.mode = "ssh-tunnel";` added (server reachable from CI).
      # See nix/ollama-shroom.nix and research/remote-ollama-ci.md in the plan directory.
      nixosModules.ollama-testing = import ./nix/ollama-shroom.nix;
    };
}
