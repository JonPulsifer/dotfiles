{ emacs }: rec {
  bruh = "fortune | cowsay -f moose | lolcat";
  ll = "exa -lg";
  la = "exa -lag";
  ls = "exa";
  paths = "env | grep ^PATH= | cut -f2 -d= | tr -s : \\\\n | sort";
  switch = "home-manager switch";
  reload = "unset __HM_SESS_VARS_SOURCED && " + switch + " && . ~/.profile";
  tree = "exa --tree";
  emacs-gui = "open ${emacs}/Applications/Emacs.app";
}
