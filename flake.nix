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
              (hfinal: hprev:
                let
                  # Upstream test suites are not ours to run, and none of these can pass
                  # in the sandbox: the baikai sdists ship neither test/fixtures nor
                  # data/models, and claude's and openai's tasty suites call getEnv on an
                  # API key and talk to the live service. Take the libraries only.
                  fromHackage = pkg: ver: sha256:
                    dontCheck (hfinal.callHackageDirect { inherit pkg ver sha256; } { });
                in
                {
                  # nixpkgs-unstable does not package the baikai family at all (checked via
                  # `nix eval`, 2026-09-29: only "claude" of the shroom-baikai dependency
                  # set is present in haskellPackages), and its own "claude" is pinned to
                  # 1.4.0, older than the 1.5.0 that baikai-claude needs. Pull all five
                  # straight from Hackage. callHackageDirect fetches with fetchzip, so
                  # every sha256 here is the *unpacked* source hash, as produced by
                  # `nix store prefetch-file --unpack`.
                  claude = fromHackage "claude" "1.5.0"
                    "sha256-Jng3pCOl9d9XqXbHomBJNiXDMViKZYymj9AKTWwHhvY=";
                  baikai-claude = fromHackage "baikai-claude" "0.7.0.0"
                    "sha256-GSOzULFgbJae4Wf6vwdr2xR+83CwhrCuWMxuZJG6NZA=";
                  baikai-effectful = fromHackage "baikai-effectful" "0.4.0.2"
                    "sha256-SgcvuYmfJt8bF4WU5MCKQzTCBkAPdsy5G5X3nwPVPd0=";
                  baikai-openai = fromHackage "baikai-openai" "0.7.0.0"
                    "sha256-FVYkPkL8hzzaNY0YprnmRFVswJKHyqEk15hcPauyYkI=";

                  # baikai-effectful 0.4.0.2 and shroom-baikai itself both want
                  # effectful ^>=2.7; nixpkgs is on 2.6.1.0.
                  effectful = fromHackage "effectful" "2.7.1.0"
                    "sha256-1jr7uWldG/qzNljv41c8ustRFNLnD9DuOFBmL3BYT6g=";
                  effectful-core = fromHackage "effectful-core" "2.7.1.2"
                    "sha256-OZhGk0UY3BMWF+oUAQnCvF3hnzscBCm0Cz+nz8p2XM8=";
                  # effectful-core 2.7 needs strict-mutable-base >=2; nixpkgs has 1.1.0.0.
                  strict-mutable-base = fromHackage "strict-mutable-base" "2.0.0.0"
                    "sha256-3o2PMN8l56X7ULqyNNJrJQZ8xgqqOsxhjm0jfULQt+k=";

                  # baikai 0.7.1.0 wants streamly >=0.11, streamly-core >=0.3 and
                  # generic-lens >=2.3; nixpkgs is still on 0.10.1 / 0.2.3 / 2.2.2.0, and
                  # so is current nixos-unstable, so bumping the pin would not have helped.
                  streamly = fromHackage "streamly" "0.11.1"
                    "sha256-4h1MwaN7eXMvzXKyjggIjjR3BlsGzl4vfCO7VBGGvrc=";
                  streamly-core = fromHackage "streamly-core" "0.3.1"
                    "sha256-k9h+I74GNsluf55hJFDZiLwEO2x9moFvtCarCeCpaa4=";
                  generic-lens = fromHackage "generic-lens" "2.3.0.0"
                    "sha256-V8M8gkbrrLAsJ42IKa26HnU28sfljwUZuBiCJBV8ABs=";
                  generic-lens-core = fromHackage "generic-lens-core" "2.3.0.0"
                    "sha256-Abntgf3UMhQed5gOc6sDoVilMc0FRRCh8VJCeoQfNRY=";

                  # baikai's remaining unmet bound is `tls >=2.2 && <2.5`, and that one is
                  # not worth satisfying: tls 2.2 needs the crypton-x509 1.8 family and tls
                  # 2.4 needs crypton >=1.1 (via mlkem, itself marked broken), so either
                  # rebuilds the whole TLS/HTTP stack. nixpkgs' tls 2.1.8 is API-compatible
                  # with what baikai actually uses -- it compiles clean against it -- so
                  # relax the bound instead.
                  baikai = doJailbreak (fromHackage "baikai" "0.7.1.0"
                    "sha256-h2BWpYua+/RT/cDBqVNeAK9XFFrD5j8FXJQh9PHMa4Y=");

                  # baikai declares `build-depends: openai ^>=2.5` -- a different Hackage
                  # package (Servant bindings to the OpenAI API, unrelated to
                  # baikai-openai). nixpkgs pins openai-2.5.3 and marks it broken; it is
                  # broken only because its tasty suite calls getEnv "OPENAI_KEY" and hits
                  # the live API, so the flag comes off together with the tests.
                  openai = dontCheck (markUnbroken hprev.openai);

                  # baikai-claude transitively pulls in cradle, also marked broken. Same
                  # story: its spec shells out to a Python toolchain and reads
                  # PYTHON_BIN_PATH, so 45 of its 51 examples fail in the sandbox while the
                  # library itself compiles fine.
                  cradle = dontCheck (markUnbroken hprev.cradle);

                  # unicode-data 0.6.0 (via streamly) asserts against Unicode 15.1.0 while
                  # GHC 9.12's base ships 16.0.0. The data tables are fine; only the
                  # assertions are stale.
                  unicode-data = dontCheck hprev.unicode-data;
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
            (mapAttrs (pname: path: disableCabalFlag (hfinal.callCabal2nix pname path { }) "online") localPackages);

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
      # See nix/ollama-shroom.nix itself for import, deployment and CI-coupling docs —
      # it is written for an agent reading only this file, in a different repository.
      nixosModules.ollama-shroom = import ./nix/ollama-shroom.nix;
    };
}
