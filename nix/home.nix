{ pkgs, lib, ... }:

let
  nixpkgs = builtins.fetchTarball (import ./nixpkgs.nix);
  name = "Jonathan Pulsifer";
  email = if pkgs.stdenv.hostPlatform.isMacOS then
      "jonathan.pulsifer@shopify.com"
    else
      "jonathan@pulsifer.ca";
  pgpkey = "29034642BD1D7CAFDC0445540472D3B3F5012430";
  github = "jonpulsifer";
  homedir = builtins.getEnv "HOME";
  dotfiles = "${homedir}/.dotfiles";
  homeutils = pkgs.callPackage ../utils { };
  commonEnv = rec {
    CLOUDSDK_CONFIG = "${homedir}/.config/gcloud";
    EDITOR = "vim";
    GIT_EDITOR = EDITOR;
    VISUAL = EDITOR;
    GO111MODULE = "on";
    GOPATH = homedir;
    KUBECONFIG =
      "${homedir}/.kube/config:${homedir}/.kube/config.shopify.cloudplatform";
    LANG = "en_US.UTF-8";
    LC_ALL = LANG;
    PATH = "${homedir}/bin:${homedir}/opt/google-cloud-sdk/bin:$PATH";
    VAULT_ADDR = "https://vault.pulsifer.ca";
    # VAULT_CACERT="${dotfiles}/ca.crt";
  };
  linuxEnv = { LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive"; };
  homeEnv = if pkgs.stdenv.hostPlatform.isMacOS then
      commonEnv
    else
      commonEnv // linuxEnv;
in {
  manual.manpages.enable = false;
  home.packages = with pkgs; [
    # packages found in ../utils
    homeutils

    # things i use
    _1password
    awscli
    ffmpeg
    gnumake
    htop
    htop
    httpie
    jq
    nmap
    unzip
    wget
    youtube-dl

    # bruh
    cowsay
    figlet
    fortune
    lolcat
    neofetch

    # rust tools are hipster
    exa
    fd
    hexyl
    ripgrep
    sd
    tokei
    xsv
  ];
  programs.bat.enable = true;
  programs.command-not-found.enable = true;
  programs.fzf.enable = true;
  programs.gpg.enable = true;
  programs.starship.enable = false;
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = name;
    userEmail = email;
    signing.key = pgpkey;
    signing.signByDefault = true;

    extraConfig = {
      color.ui = true;
      core = { whitespace = "trailing-space,space-before-tab"; };
      format = { signoff = true; };
      github.user = github;
      help = { autocorrect = 1; };
      hub.protocol = "https";
      pull = { ff = "only"; };
      pull.rebase = true;
      push = { default = "current"; };
      url."https://github.com/Shopify/".insteadOf = [
        "git@github.com:Shopify/"
        "git@github.com:shopify/"
        "ssh://git@github.com:Shopify/"
        "ssh://git@github.com:Shopify/"
      ];
      url."git@github.com:jonpulsifer/".insteadOf =
        [ "git@github.com:jonpulsifer/" "https://github.com/jonpulsifer/" ];
      url."git@github.com:CovidShield/".insteadOf =
        [ "https://github.com/covidshield/" "https://github.com/CovidShield/" ];
    };

    aliases = {
      co = "checkout";
      d = "diff";
      s = "status";
      f = "fetch";
    };

    ignores = [
      ".DS_Store"
      "*~"
      "*.swp"
      "*_rsa"
      "*_ed25519"
      "*.pub"
      "credentials.json"
      "secrets*.json"
    ];
  };
  programs.bash = {
    enable = true;
    historySize = 1000000;
    historyFileSize = 1000000;
    historyFile = "${homedir}/.bash_history";
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
    historyIgnore = [ "ls" "ll" "cd" "exit" "pwd" ];

    sessionVariables = homeEnv;
    shellAliases = import ../home/aliases.nix;
    profileExtra = ''
      export PS1="\[\e[34;1m\]\w \[\e[37;1m\]$\[\e[m\] "
      declare -a files=(
        ${homedir}/.nix-profile/etc/profile.d/nix.sh
        ${homedir}/.nix-profile/share/bash-completion/bash_completion
        /usr/local/etc/bash_completion
        /opt/dev/dev.sh
      )
      for index in ''${!files[*]}; do
        if [[ -r ''${files[$index]} ]]; then
          source ''${files[$index]}
        fi
      done
    '';
  };
  programs.zsh = {
    enable = false;
    enableCompletion = true;
    enableAutosuggestions = true;
    history = {
      path = "${homedir}/.zsh_history";
      size = 50000;
      save = 50000;
    };
    sessionVariables = homeEnv;
    shellAliases = import ../home/aliases.nix;
    defaultKeymap = "emacs";
    initExtraBeforeCompInit = ''
      setopt prompt_subst
      setopt AUTO_CD
      setopt EXTENDED_GLOB
      setopt NOMATCH
      setopt PROMPT_SP
      setopt auto_pushd
      setopt append_history
    '';
    initExtra = ''
      autoload -Uz promptinit
      promptinit
      # zsh-mime-setup
      autoload colors
      colors
      zstyle ':completion:*' special-dirs true
      zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,comm'
      zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
      if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
        source ~/.nix-profile/etc/profile.d/nix.sh
      fi
      if [ -f /opt/dev/dev.sh ]; then
        source /opt/dev/dev.sh
      fi
    '';

    plugins = [
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.6.4";
          sha256 = "1h8h2mz9wpjpymgl2p7pc146c1jgb3dggpvzwm9ln3in336wl951";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.8.0-alpha1-pre-redrawhook";
          sha256 = "0w8x5ilpwx90s2s2y56vbzq92ircmrf0l5x8hz4g1nx3qzawv6az";
        };
      }
    ];
  };
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = builtins.readFile ../home/vimrc;
    plugins = with pkgs.vimPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      gitgutter
      nerdtree
      papercolor-theme
      surround
      vim-codefmt
      vim-endwise
      vim-startify

      vim-go
      vim-javascript
      vim-javascript-syntax
      vim-jsbeautify
      vim-json
      vim-markdown
      vim-nix
      vim-ruby
      vim-tsx
      vim-terraform
    ];
  };
  programs.tmux = {
    enable = true;
    baseIndex = 1;
    shortcut = "g";
    escapeTime = 0;
    historyLimit = 500000;
    terminal = "screen-256color";
    extraConfig = builtins.readFile ../home/tmux.conf
      + (if pkgs.stdenv.isDarwin then
        builtins.readFile ../home/tmux.darwin.conf
      else
        "");
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.mkDerivation {
          pluginName = "1password";
          rtpFilePath = "plugin.tmux";
          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "yardnsm";
            repo = "tmux-1password";
            rev = "d541aa3cd44417d314a13216b9621d9b6c88235d";
            sha256 = "0531wc2sh22ip1yh639nhlzw7w4jmki4jc6rhwyv0n4vkbajlz1b";
          };
        };
        extraConfig = ''
          set -g @1password-subdomain 'pulsifer'
          set -g @1password-vault 'afh4hmnamahifeepetyjloissq'
          set -g @1password-key 'x'
          set -g @1password-copy-to-clipboard 'off'
        '';
      }
      tmuxPlugins.tmux-fzf
    ];
    secureSocket = false;
  };

  services.gpg-agent = if pkgs.hostPlatform.isMacOS then {
    enable = false;
  } else {
    enable = true;
    pinentryFlavor = "curses";
    defaultCacheTtl = 86400;
    maxCacheTtl = 7200;
    enableSshSupport = true;
    sshKeys = [ "3BF5FE568B9965E185EB48887269D6494CD87EC5" ];
    enableExtraSocket = true;
    maxCacheTtlSsh = 7200;
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };
}
