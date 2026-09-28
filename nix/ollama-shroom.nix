{ config, lib, ... }:

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
    enable = lib.mkEnableOption "shroom's test Ollama: same model, kept resident, on the laptop or a server reachable from CI";

    model = lib.mkOption {
      type = lib.types.str;
      default = "qwen3:8b";
      description = ''
        The model pulled on activation and kept resident. `llama3.2:3b` is not a
        defensible default here — it is weak at both structured output and tool calls,
        the two things shroom leans on hardest.

        This option does not, by itself, make shroom's integration suite ask for this
        model: `OLLAMA_MODEL` still has to be set to match, which is CI's job (a later
        todo) rather than this module's.
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
      environmentVariables.OLLAMA_KEEP_ALIVE = "-1";
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
