{ config, lib, pkgs, ... }:

# shroom's test Ollama — the three names this module goes by.
#
# The same module is reached three different ways, by design, and each name is opaque
# to someone who only has one of them:
#   - file:         nix/ollama-shroom.nix (this file, in the shroom repo)
#   - flake output: `nixosModules.ollama-shroom`
#   - option:       `services.ollama.shroom`
#
# ## Importing it, including from this arc's still-open, unmerged branch
#
#   inputs.shroom.url = "github:turion/shroom/turion/shroom-layer";  # branch, while unmerged
#   imports = [ inputs.shroom.nixosModules.ollama-shroom ];
#
# Switch the `url` to drop the branch suffix once the arc lands on `main`.
#
# ## A pasteable server example (reached by CI through an SSH tunnel)
#
#   services.ollama.shroom = {
#     enable = true;
#     model = "llama3.2:1b";               # must match .github/workflows/ci.yml's OLLAMA_MODEL — see below
#     # keepAlive = "-1";                   # redundant: auth.mode = "ssh-tunnel" already defaults it to "-1" (resident, never reloaded between runs); set it only to override
#     auth.mode = "ssh-tunnel";
#     auth.sshTunnel.publicKey = "ssh-ed25519 AAAA... ci";  # public half only; the private half is a GitHub secret, see below
#   };
#
# ## A pasteable laptop example (no GPU: CPU-only)
#
#   services.ollama.shroom = {
#     enable = true;
#     model = "qwen3:8b";                   # CPU-only: a laptop has RAM an 8B model can use
#     # auth.mode defaults to "local", and in that mode keepAlive defaults to
#     # Ollama's own upstream default ("5m") rather than staying resident forever,
#     # so the model doesn't sit pinned in RAM between sessions. (Only "ssh-tunnel"
#     # mode defaults keepAlive to "-1".)
#   };
#
# ## A pasteable laptop example with an NVIDIA GPU (~4 GB VRAM)
#
# This module sets no `services.ollama.package`, so the host chooses GPU acceleration
# itself. NixOS 26.05 removed `services.ollama.acceleration`; pick the package instead
# (`pkgs.ollama-{cpu,cuda,rocm,vulkan}`):
#
#   services.ollama.package = pkgs.ollama-cuda;
#   services.ollama.environmentVariables = {
#     OLLAMA_CONTEXT_LENGTH = "8192";       # shroom's prompts run ~4k tokens, over Ollama's default 4096
#     OLLAMA_FLASH_ATTENTION = "1";
#     OLLAMA_KV_CACHE_TYPE = "q8_0";        # quantised KV cache, to fit a small card
#   };
#   services.ollama.shroom = {
#     enable = true;
#     model = "qwen3:4b-instruct-2507-q4_K_M";  # fits a ~4 GB card fully, see below
#     gpuLayers = 99;                       # all layers on the GPU; see `gpuLayers` and `gpuModel`
#   };
#
# With `gpuLayers` set, run local tests with `OLLAMA_MODEL=qwen3:4b-instruct-2507-q4_K_M-gpu`
# (`<model>-gpu`, also readable as `config.services.ollama.shroom.gpuModel`).
#
#   - `pkgs.ollama-cuda` needs unfree CUDA libraries allowed
#     (`nixpkgs.config.allowUnfree = true`, or a narrower `allowUnfreePredicate`).
#   - It also needs `cache.nixos-cuda.org` as a substituter (with its public key in
#     `trusted-public-keys`), or ollama-cuda builds from source.
#   - Prefer the `instruct-2507` variant: the qwen3 base tags emit `<think>` blocks
#     (`qwen3:0.6b` took 149 s on one run for that reason).
#   - After the first request, check that `/api/ps` reports `size_vram == size` for the
#     model. If it doesn't, the model has spilled over to the CPU, and that is silent:
#     nothing errors, it is just slow. Pick a smaller model or quantisation.
#   - Even when the model fits, llama.cpp's fit step keeps a fixed 1024 MiB of VRAM free, so a
#     small card ends up half-used (30 of 37 layers here). `gpuLayers` overrides that.
#
# ## An Anthropic Messages API in front of it (development and testing)
#
# One more line gives the same machine Bifrost, speaking the Anthropic Messages API and
# forwarding to this module's own Ollama:
#
#   services.ollama.shroom.anthropicProxy.enable = true;
#
# A shroom program then reaches it with:
#
#   anthropicCompatBackend "http://127.0.0.1:8787/anthropic" "ollama/llama3.2:1b" "unused"
#
#   - The base URL has NO trailing `/v1` (baikai strips one and appends its own) but DOES end in
#     `/anthropic`, Bifrost's Anthropic route. The model id carries the `ollama/` provider prefix.
#   - The port is `anthropicProxy.port` (default 8787). It is loopback-only, never opened in the
#     firewall, and meant for development and testing, not for serving anyone else.
#   - The credential is a placeholder, not a secret. The module generates a `config.json` in the
#     Nix store (world-readable) whose one provider is `ollama`, with an upstream key named
#     `ollama-shroom` whose value is `unused`: Bifrost insists on a key entry, Ollama checks none.
#     Bifrost verifies no credential on its inference routes, so a client may send any non-empty
#     `x-api-key` (baikai refuses to dispatch with none); `unused` is the conventional one.
#   - A `preStart` step installs that file as `config.json` in the unit's `StateDirectory=`
#     (`/var/lib/shroom-anthropic-proxy`), which is also Bifrost's `-app-dir`, after deleting
#     Bifrost's sqlite `config.db` there, so `config.json` is the only source of truth and an
#     old `services.ollama.host`/`port` can never survive in a stale database. `logs.db` is kept.
#   - A tool loop does not work through this stack: Ollama applies the request's JSON schema as
#     a grammar to the whole reply, so with a tool and a schema in the same request no
#     `tool_use` is ever emitted. Use it for single-shot structured output only.
#
# **An importer's nixpkgs must provide `go_1_27`** (Bifrost 2.x declares `go 1.27`), or set
# `services.ollama.shroom.anthropicProxy.package` to a Bifrost it builds itself. Otherwise
# enabling the proxy is an evaluation error; with the proxy disabled (the default) the package
# option is not even forced. The default package is the derivation vendored in `nix/bifrost.nix`,
# which also says what retires it.
#
# ## The one coupling that breaks CI if ignored
#
# `services.ollama.shroom.model` here and `OLLAMA_MODEL` in `.github/workflows/ci.yml` must
# name the exact same tag. A mismatch doesn't fail loudly — CI's suite falls back to a
# model the server never pulled and fails with `not_found_error`, which reads as a shroom
# bug rather than a configuration one.
#
# ## What must exist on GitHub, by exact name and kind
#
#   - a **secret** named `OLLAMA_SSH_KEY` — the *private* half of the tunnel key above.
#   - a **repository variable** (not a secret) named `OLLAMA_SERVER_HOST` — the workflow
#     reads `vars.OLLAMA_SERVER_HOST`; setting this as a secret instead yields an empty
#     host and a failing `ssh-keyscan`. This has already happened once.
#
# ## Sizing
#
# In `ssh-tunnel` mode the model is held resident once loaded (see `keepAlive`), so pick
# one that fits the host: a 2-core / 12 GB server wants a 1b-class model; `qwen3:8b` is
# the choice for a CPU-only laptop. A host with a GPU should instead pick a model that
# fits its VRAM fully, since spillover to the CPU is silent; see the GPU laptop example
# above.
let
  cfg = config.services.ollama.shroom;

  # `command=""` denies a shell; `permitopen` is the whole access-control story and it
  # is enforced by sshd, not by anything shroom or Ollama do. See
  # research/remote-ollama-ci.md in the plan directory for the mechanism and threat model.
  authorizedKeyOptions = lib.concatStringsSep "," [
    ''command=""''
    "no-pty"
    "no-agent-forwarding"
    "no-X11-forwarding"
    ''permitopen="127.0.0.1:${toString config.services.ollama.port}"''
  ];

  proxy = cfg.anthropicProxy;
  proxyName = "shroom-anthropic-proxy";

  # The placeholder credential: Bifrost wants a key entry for the provider, Ollama checks none.
  # It lands in the Nix store, so it must stay a dummy.
  placeholderKey = "unused";

  # `models = [ "*" ]`: Bifrost denies every model to a key that lists none.
  proxyConfigFile = (pkgs.formats.json { }).generate "bifrost-config.json" {
    providers.ollama.keys = [
      {
        name = "ollama-shroom";
        value = placeholderKey;
        models = [ "*" ];
        weight = 1;
        ollama_key_config.url = "http://${config.services.ollama.host}:${toString config.services.ollama.port}";
      }
    ];
  };
in
{
  options.services.ollama.shroom = {
    enable = lib.mkEnableOption "shroom's test Ollama: the same model, on the laptop or a server reachable from CI — see `keepAlive` for whether it stays resident between runs";

    model = lib.mkOption {
      type = lib.types.str;
      default = "llama3.2:1b";
      description = ''
        The model pulled on activation. `llama3.2:3b` is not a defensible default here
        — it is weak at both structured output and tool calls, the two things shroom
        leans on hardest. The default above is the CI-server size: the server CI
        tunnels to is 2 cores / 12 GB with no GPU, where an 8B model runs at roughly
        1-2 tokens/second. A CPU-only laptop has room for more — set this to
        `"qwen3:8b"` explicitly for that case. A laptop with a GPU should instead pick
        a model that fits its VRAM fully, because spillover to the CPU is silent: for a
        ~4 GB NVIDIA card, `"qwen3:4b-instruct-2507-q4_K_M"`. See the GPU laptop
        example at the top of `nix/ollama-shroom.nix`.

        This option does not, by itself, make shroom's integration suite ask for this
        model: `OLLAMA_MODEL` in `.github/workflows/ci.yml` still has to be set to the
        same tag, or the suite fails against a model the server never pulled.
      '';
    };

    keepAlive = lib.mkOption {
      type = lib.types.str;
      default = if cfg.auth.mode == "ssh-tunnel" then "-1" else "5m";
      defaultText = lib.literalExpression ''if config.services.ollama.shroom.auth.mode == "ssh-tunnel" then "-1" else "5m"'';
      example = "-1";
      description = ''
        Value for `OLLAMA_KEEP_ALIVE`. The default follows `auth.mode`:

        - `auth.mode = "ssh-tunnel"` (a dedicated host CI tunnels to) defaults to
          `"-1"`, so the model stays resident and is never reloaded between test
          runs. Left to Ollama's own default, every CI run started cold and paid
          roughly 19 seconds of model load before the first token.
        - `auth.mode = "local"` (the laptop) defaults to `"5m"`, Ollama's own upstream
          behaviour (unload after 5 minutes idle): `"-1"` on a laptop pins a multi-GB
          model in RAM indefinitely — the maintainer has had to stop Ollama by hand
          over exactly this.

        One module serves both machines and they want opposite things here, so the
        mode picks the default. Setting this option explicitly on a machine overrides
        the default in either mode.
      '';
    };

    maxLoadedModels = lib.mkOption {
      type = lib.types.ints.positive;
      default = 1;
      description = ''
        Value for `OLLAMA_MAX_LOADED_MODELS`. Caps how many distinct models Ollama
        keeps loaded at once, so a second model pulled later can never silently
        double the resident footprint alongside the one this module manages.
      '';
    };

    gpuLayers = lib.mkOption {
      type = lib.types.nullOr lib.types.ints.unsigned;
      default = null;
      example = 99;
      description = ''
        Number of model layers to put on the GPU (Ollama's `num_gpu`), or `null` to leave
        Ollama's own choice alone. Ollama's fit step keeps a fixed amount of VRAM free
        (1024 MiB), so on a small card (~4 GB) it offloads only part of a model that would
        fit entirely, which makes it several times slower. `99` means "all layers".
        Only makes sense with a GPU `services.ollama.package` (e.g. `pkgs.ollama-cuda`).

        shroom talks Ollama's OpenAI-compatible API, which cannot pass `num_gpu` per
        request. So when this is set, a oneshot unit creates an alias model
        `<model>-gpu` (readable as `gpuModel`) with that `num_gpu` once the base model
        has been pulled; point `OLLAMA_MODEL` at the alias.
      '';
    };

    gpuModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      readOnly = true;
      default = if cfg.gpuLayers == null then null else "${cfg.model}-gpu";
      defaultText = lib.literalExpression ''if config.services.ollama.shroom.gpuLayers == null then null else "''${config.services.ollama.shroom.model}-gpu"'';
      description = ''
        Name of the alias model created when `gpuLayers` is set (`null` otherwise). Set
        `OLLAMA_MODEL` to this for local runs.
      '';
    };

    auth.mode = lib.mkOption {
      type = lib.types.enum [ "local" "ssh-tunnel" ];
      default = "local";
      description = ''
        `"local"`: nothing beyond Ollama itself, bound to `127.0.0.1` — the laptop case.

        `"ssh-tunnel"`: additionally creates a dedicated, unprivileged user whose sole
        authorised key may forward to Ollama's port and do nothing else. Intended for
        the maintainer's server, reached by CI through an SSH tunnel.
      '';
    };

    auth.sshTunnel.publicKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = "ssh-ed25519 AAAA... ci";
      description = ''
        Public half of the SSH key CI tunnels in with. Only ever the public key: the
        restriction lives in `permitopen` and `command=""`, enforced by sshd, so nothing
        secret needs to live on the server at all.
      '';
    };

    anthropicProxy.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      example = true;
      description = ''
        Also run Bifrost in front of this module's Ollama, speaking the Anthropic
        Messages API, for development and testing of Anthropic-API clients (shroom's
        `anthropicCompatBackend`) without an Anthropic account. Loopback-only; the API
        key is a placeholder (`unused`), not a secret. A tool loop does not work through
        it (Ollama grammar-constrains the whole reply). Defaults to `false`.

        An importing configuration's nixpkgs must provide `go_1_27`, or set
        `anthropicProxy.package` to a Bifrost it builds itself; otherwise enabling this
        is an evaluation error. The URL, model id and the rest are in the header of
        `nix/ollama-shroom.nix`.
      '';
    };

    anthropicProxy.port = lib.mkOption {
      type = lib.types.port;
      default = 8787;
      description = ''
        TCP port Bifrost listens on when `anthropicProxy.enable` is set. Loopback-only,
        never opened in the firewall, for development and testing. How a client
        reaches it is in the header of `nix/ollama-shroom.nix`.
      '';
    };

    anthropicProxy.package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ./bifrost.nix { };
      defaultText = lib.literalExpression "pkgs.callPackage ./bifrost.nix { }";
      description = ''
        The Bifrost to run. Defaults to the derivation vendored in `nix/bifrost.nix`
        (see there for where it comes from and what retires it), which needs a nixpkgs
        with `go_1_27`; set this to supply your own otherwise.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.ollama = {
      enable = true;
      # Never a public interface, in either mode.
      host = "127.0.0.1";
      environmentVariables = {
        OLLAMA_KEEP_ALIVE = cfg.keepAlive;
        OLLAMA_MAX_LOADED_MODELS = toString cfg.maxLoadedModels;
      };
      # nixpkgs' own model loader: a systemd unit ordered after ollama.service that
      # pulls each listed model once it's up.
      loadModels = [ cfg.model ];
    };

    # The alias carrying `num_gpu`. Ordered after nixpkgs' loader, but that unit is
    # `Type=exec` and keeps pulling after it has started, so the script polls `/api/show`
    # (bounded) for the base model rather than trusting the ordering. A timeout fails the
    # unit and `Restart` tries again; `/api/create` overwrites, so re-running is harmless.
    systemd.services.ollama-shroom-gpu-alias = lib.mkIf (cfg.gpuLayers != null) {
      description = "Create the ${cfg.gpuModel} alias with num_gpu = ${toString cfg.gpuLayers}";
      wantedBy = [ "multi-user.target" "ollama.service" ];
      requires = [ "ollama.service" ];
      wants = [ "ollama-model-loader.service" ];
      after = [ "ollama.service" "ollama-model-loader.service" ];
      bindsTo = [ "ollama.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        DynamicUser = true;
        Restart = "on-failure";
        RestartSec = "30s";
      };
      script =
        let
          curl = lib.getExe pkgs.curl;
          api = "http://${config.services.ollama.host}:${toString config.services.ollama.port}/api";
          show = lib.escapeShellArg (builtins.toJSON { model = cfg.model; });
          create = lib.escapeShellArg (builtins.toJSON {
            model = cfg.gpuModel;
            from = cfg.model;
            parameters.num_gpu = cfg.gpuLayers;
            stream = false;
          });
        in
        ''
          for _ in $(seq 60); do
            if '${curl}' --silent --fail --output /dev/null --data ${show} '${api}/show'; then
              exec '${curl}' --silent --show-error --fail --data ${create} '${api}/create'
            fi
            sleep 5
          done
          echo "${cfg.model} still not available after 5 minutes" >&2
          exit 1
        '';
    };

    # Bifrost in front of this module's Ollama, speaking the Anthropic Messages API. Modelled
    # on nixpkgs PR 521236's `nixos/modules/services/misc/bifrost.nix` (same hardening, same
    # `StateDirectory=` + `-app-dir` shape), but fixed to this one use: loopback, one provider.
    systemd.services.${proxyName} = lib.mkIf proxy.enable {
      description = "Bifrost, an Anthropic Messages API in front of shroom's test Ollama";
      wantedBy = [ "multi-user.target" "ollama.service" ];
      # The upstream is this module's own Ollama.
      requires = [ "ollama.service" ];
      bindsTo = [ "ollama.service" ];
      # Bifrost fetches its pricing datasheets over HTTPS at startup and exits when that fails
      # with nothing cached (PR 521236), so wait for a working network, not a merely configured one.
      wants = [ "network-online.target" ];
      after = [ "network-online.target" "ollama.service" ];

      # `config.db` and its WAL sidecars (sqlite WAL mode leaves them after an unclean stop) are
      # dropped so `config.json` stays authoritative; this module is fully declarative, so the
      # database holds nothing worth keeping. `logs.db*` stays.
      preStart = ''
        rm -f "$STATE_DIRECTORY"/config.db "$STATE_DIRECTORY"/config.db-wal "$STATE_DIRECTORY"/config.db-shm
        install -Dm600 '${proxyConfigFile}' "$STATE_DIRECTORY/config.json"
      '';

      startLimitIntervalSec = 300;
      startLimitBurst = 5;

      serviceConfig = {
        Restart = "on-failure";
        RestartSec = 10;

        # `-host` is explicit and not left to Bifrost's own default.
        ExecStart = lib.concatStringsSep " " [
          (lib.getExe proxy.package)
          "-host"
          "127.0.0.1"
          "-port"
          (toString proxy.port)
          "-app-dir"
          "%S/${proxyName}"
          "-log-level"
          "info"
          "-log-style"
          "json"
        ];

        # `config.db` and `logs.db` (sqlite) land in `-app-dir`, so it has to be writable by
        # the dynamic user: /var/lib/${proxyName}.
        StateDirectory = proxyName;
        WorkingDirectory = "%S/${proxyName}";

        DynamicUser = true;
        DevicePolicy = "closed";
        LockPersonality = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SystemCallArchitectures = "native";
        UMask = "0077";
      };
    };

    users.users = lib.mkIf (cfg.auth.mode == "ssh-tunnel") {
      ci-ollama = {
        isNormalUser = true;
        openssh.authorizedKeys.keys =
          lib.optional (cfg.auth.sshTunnel.publicKey != "")
            "${authorizedKeyOptions} ${cfg.auth.sshTunnel.publicKey}";
      };
    };

    assertions = [
      {
        assertion = cfg.auth.mode != "ssh-tunnel" || cfg.auth.sshTunnel.publicKey != "";
        message = "services.ollama.shroom.auth.mode is \"ssh-tunnel\" but auth.sshTunnel.publicKey is unset — the tunnel user would have no authorised key.";
      }
    ];
  };
}
