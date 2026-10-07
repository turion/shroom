{ config, lib, ... }:

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
# ## A pasteable laptop example
#
#   services.ollama.shroom = {
#     enable = true;
#     model = "qwen3:8b";                   # a laptop has headroom an 8B model can use
#     # auth.mode defaults to "local", and in that mode keepAlive defaults to
#     # Ollama's own upstream default ("5m") rather than staying resident forever,
#     # so the model doesn't sit pinned in RAM between sessions. (Only "ssh-tunnel"
#     # mode defaults keepAlive to "-1".)
#   };
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
# one that fits the host: a 2-core / 12 GB server wants a 1b-class model; `qwen3:8b` is the laptop default.
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
        1-2 tokens/second. A laptop has room for more — set this to `"qwen3:8b"`
        explicitly for that case.

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
