{ pkgs, ... }:

let
  # it me
  name = "Jonathan Pulsifer";
  email = if pkgs.stdenv.hostPlatform.isMacOS then
    "jonathan.pulsifer@shopify.com"
  else
    "jonathan@pulsifer.ca";
  pgpkey = "29034642BD1D7CAFDC0445540472D3B3F5012430";
  github = "jonpulsifer";

  # environment
  homedir = builtins.getEnv "HOME";
  dotfiles = "${homedir}/.dotfiles";
  commonEnv = rec {
    CLOUDSDK_CONFIG = "${homedir}/.config/gcloud";
    EDITOR = "emacs";
    GIT_EDITOR = EDITOR;
    VISUAL = "emacs";
    GO111MODULE = "on";
    GOPATH = homedir;
    KUBECONFIG =
      "${homedir}/.kube/config:${homedir}/.kube/config.shopify.cloudplatform";
    LANG = "en_US.UTF-8";
    LC_ALL = LANG;
    PATH =
      "${homedir}/bin:${homedir}/opt/google-cloud-sdk/bin:${homedir}/node_modules/.bin:$PATH";
    PROMPT_DIRTRIM = 3;
    VAULT_ADDR = "https://vault.pulsifer.ca";
    # VAULT_CACERT="${dotfiles}/ca.crt";
  };
  linuxEnv = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
  homeEnv = if pkgs.stdenv.hostPlatform.isMacOS then
    commonEnv
  else
    commonEnv // linuxEnv;

  # custom packages
  shell-utils = pkgs.callPackage ../src/shell-utils { };
  ddnsb0t = pkgs.callPackage ../src/ddnsb0t { };
  emacs = if pkgs.stdenv.isDarwin then pkgs.emacsMacport else pkgs.emacs;

in {
  manual.manpages.enable = false;
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # fonts
    fira-code

    # packages found in ../src
    shell-utils
    ddnsb0t

    # languages
    go
    nodejs
    ruby
    yarn

    # things i use
    _1password
    awscli
    ffmpeg
    gnumake
    htop
    httpie
    jq
    kubectl
    ngrok
    nixfmt
    # nix-linter
    nmap
    shellcheck
    unzip
    vault
    wget
    # wol
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

  home.file.".emacs.d/init.el".source = ../src/init.el;
  home.file.".emacs.d/lisp/ws-trim.el".source = ../src/ws-trim.el;
  programs.emacs = {
    enable = true;
    package = emacs;
  };

  programs.fzf = {
    enable = true;
    defaultCommand = "${pkgs.fd}/bin/fd --type f";
    defaultOptions = [
      "--reverse"
      "--info=inline"
      "--border"
      "--height=40%"
      "--margin=0,50,0,0"
    ];
  };
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
      "\\#*\\#"
      "*~"
    ];
  };
  programs.bash = {
    enable = true;
    historySize = 1000000;
    historyFileSize = 1000000;
    historyFile = "${homedir}/.bash_history";
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
    historyIgnore = [ "ls" "ll" "cd" "exit" "pwd" "neofetch" "bruh" "-l" ];

    sessionVariables = homeEnv;
    shellAliases = import ../src/aliases.nix { emacs = emacs; };
    profileExtra = ''
      export PS1="\[\e[34;1m\]\u\[\e[37;1m\]@\[\e[36;1m\]\h\[\e[37;1m\]:\[\e[34;1m\]\w \[\e[37;1m\]$\[\e[m\] "
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
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = builtins.readFile ../src/vimrc;
    plugins = with pkgs.vimPlugins;
      [
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

        vim-javascript
        vim-javascript-syntax
        vim-jsbeautify
        vim-json
        vim-markdown
        vim-nix
        vim-ruby
        vim-tsx
        vim-terraform

        coc-fzf
        coc-eslint
        coc-highlight
        coc-git
        coc-json
        coc-markdownlint
        coc-nvim
        coc-python
        coc-solargraph
        coc-tslint
        coc-tsserver
        coc-yaml
        coc-yank
      ] ++ (if !pkgs.stdenv.isAarch64 then [ vim-go coc-go ] else [ ]);
  };
  programs.tmux = {
    enable = true;
    baseIndex = 1;
    shortcut = "g";
    escapeTime = 0;
    historyLimit = 500000;
    terminal = "xterm-256color";
    extraConfig = builtins.readFile ../src/tmux.conf
      + (if pkgs.stdenv.isDarwin then
        builtins.readFile ../src/tmux.darwin.conf
      else
        "");
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.mkDerivation {
          pluginName = "1password";
          rtpFilePath = "plugin.tmux";
          version = "master";
          src = fetchFromGitHub {
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
           set -g @1password-items-jq-filter '\
             .[] \
             | [select(.overview.tags | map(select(. == "tmux")) | length == 1)?] \
             | map([ .overview.title, .uuid ] \
             | join(",")) \
             | .[] \
          '
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
