# Bifrost (maximhq/bifrost), the Anthropic-format -> Ollama gateway the Anthropic contract test
# runs against. Reached as the flake package `.#bifrost` (`nix build .#bifrost`).
#
# ## Where this comes from, and what retires it
#
# Copied from nixpkgs PR 521236, "bifrost: init at 2.2.1; nixos/bifrost: init" (open, not merged,
# as of 2026-10-12), file `pkgs/by-name/bi/bifrost/package.nix` at PR head
# c7cb7179a897e915ea03af3d5cd1fa0207b4f2fd. Only the package is vendored, not the PR's NixOS module.
# `version`, `src.hash`, `npmDepsHash` and `vendorHash` are the PR's own values, unchanged.
# Local differences: `nixosTests` and `nix-update-script` are dropped from `passthru` (this repo has
# neither the PR's nixos test nor an updater), and the argument list is for `callPackage`.
#
# **When PR 521236 merges, delete this file and the `bifrost` entry in flake.nix in favour of
# `pkgs.bifrost`.** Nothing else in this repository should need to change.
#
# **Name collision:** nixpkgs PR 548274 is also titled "bifrost: init", but it packages an
# unrelated Gradle/Java project. It is not this Bifrost and has nothing to do with this file.
#
# ## Why it is not a plain buildGoModule
#
#   - `//go:embed all:ui` in bifrost-http needs a React dashboard built first, hence the separate
#     `buildNpmPackage` for `ui` below (`build-enterprise`, because the default `build` script also
#     copies into the Go tree, outside the source root).
#   - `buildGo127Module`: `transports/v2.x` declares `go 1.27.x`. Needs a nixpkgs with go_1_27.
#   - the workspace `replace` directives are re-declared in `postPatch` so sibling modules resolve.
{ lib
, buildGo127Module
, buildNpmPackage
, fetchFromGitHub
, pkg-config
, sqlite
}:

let
  version = "2.2.1";

  src = fetchFromGitHub {
    owner = "maximhq";
    repo = "bifrost";
    tag = "transports/v${version}";
    hash = "sha256-rTxY3A5TT60DmGMRIsX8KgNQgDH2swoj3+Wa/fHVTUI=";
  };

  ui = buildNpmPackage {
    pname = "bifrost-ui";
    inherit version src;
    sourceRoot = "${src.name}/ui";

    npmDepsHash = "sha256-cOswnT4ZahWX66h9oiw4t3r5GZeOH/yjbnTCAsjVgnw=";

    # The default `build` script also copies its output into the Go tree, which
    # lies outside this source root; `build-enterprise` only runs the build.
    npmBuildScript = "build-enterprise";

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/ui"
      cp -R --no-preserve=all out/. "$out/ui/"
      runHook postInstall
    '';
  };
in
buildGo127Module (finalAttrs: {
  pname = "bifrost";
  inherit version src;

  __structuredAttrs = true;

  modRoot = "transports";
  subPackages = [ "bifrost-http" ];
  vendorHash = "sha256-/3JC12PPx/RcyRyxTAZ4J06m1+o9xvg89JmI4agCkGw=";

  doCheck = false;

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ sqlite ];

  env.CGO_ENABLED = "1";

  # The `transports` module references sibling modules via `replace` directives
  # that point at relative paths inside the workspace. Re-declare them here so
  # buildGoModule resolves them from the unpacked source tree.
  postPatch = ''
    cat >> transports/go.mod <<'EOF'

    replace github.com/maximhq/bifrost/core => ../core
    replace github.com/maximhq/bifrost/framework => ../framework
    replace github.com/maximhq/bifrost/plugins/compat => ../plugins/compat
    replace github.com/maximhq/bifrost/plugins/governance => ../plugins/governance
    replace github.com/maximhq/bifrost/plugins/logging => ../plugins/logging
    replace github.com/maximhq/bifrost/plugins/maxim => ../plugins/maxim
    replace github.com/maximhq/bifrost/plugins/mocker => ../plugins/mocker
    replace github.com/maximhq/bifrost/plugins/modelcatalogresolver => ../plugins/modelcatalogresolver
    replace github.com/maximhq/bifrost/plugins/otel => ../plugins/otel
    replace github.com/maximhq/bifrost/plugins/prompts => ../plugins/prompts
    replace github.com/maximhq/bifrost/plugins/routing => ../plugins/routing
    replace github.com/maximhq/bifrost/plugins/semanticcache => ../plugins/semanticcache
    replace github.com/maximhq/bifrost/plugins/telemetry => ../plugins/telemetry
    EOF
  '';

  # `goModules` inherits `postPatch` on its own, but also `preBuild`, which the
  # module fetcher has no use for: it would pull the whole UI into the fetcher
  # and rebuild it on every UI change.
  overrideModAttrs = _: _: {
    preBuild = "";
  };

  # Provide the pre-built UI for //go:embed all:ui inside bifrost-http.
  preBuild = ''
    rm -rf bifrost-http/ui
    mkdir -p bifrost-http/ui
    cp -R --no-preserve=all ${ui}/ui/. bifrost-http/ui/
  '';

  ldflags = [
    "-s"
    "-w"
    "-X main.Version=${version}"
  ];

  passthru = { inherit ui; };

  meta = {
    description = "High-performance LLM gateway with native MCP support";
    longDescription = ''
      Bifrost is an OpenAI-compatible HTTP gateway that unifies access to 15+
      LLM providers, exposes Model Context Protocol (MCP) servers behind a
      single endpoint, and adds semantic caching, automatic failover and
      weighted key selection.
    '';
    homepage = "https://github.com/maximhq/bifrost";
    changelog = "https://github.com/maximhq/bifrost/releases/tag/transports/v${version}";
    license = lib.licenses.asl20;
    mainProgram = "bifrost-http";
    platforms = lib.platforms.linux;
  };
})
