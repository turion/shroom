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
        shroom = ./.;
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
          dependenciesOverrides = with pkgs.haskell.lib;
            composeManyExtensions [
              (hfinal: hprev: {
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

          # Start a local Ollama server with llama3.2:3b pre-pulled.
          # Usage: nix run .#ollama-server
          # Then in another terminal: cabal test shroom-test-integration-ollama
          apps.ollama-server = {
            type = "app";
            program = toString (pkgs.writeShellScript "ollama-server" ''
              export OLLAMA_MODELS="''${OLLAMA_MODELS:-$HOME/.ollama/models}"
              export OLLAMA_HOST="''${OLLAMA_HOST:-127.0.0.1:11434}"

              echo "Starting ollama server..."
              ${pkgs.ollama}/bin/ollama serve &
              OLLAMA_PID=$!

              echo "Waiting for ollama to be ready..."
              for i in $(${pkgs.coreutils}/bin/seq 1 30); do
                if ${pkgs.curl}/bin/curl -sf "http://''${OLLAMA_HOST}/api/tags" > /dev/null 2>&1; then
                  echo "Ollama is ready."
                  break
                fi
                sleep 1
              done

              echo "Pulling llama3.2:3b (this may take a while on first run)..."
              ${pkgs.ollama}/bin/ollama pull llama3.2:3b

              echo ""
              echo "Ollama server is running."
              echo "  Model: llama3.2:3b"
              echo "  Host:  http://''${OLLAMA_HOST}"
              echo ""
              echo "Run integration tests with:"
              echo "  cabal test shroom-test-integration-ollama"
              echo ""
              echo "Press Ctrl+C to stop."
              wait $OLLAMA_PID
            '');
          };
        }) // {
      inherit supportedGhcs;
    };
}
