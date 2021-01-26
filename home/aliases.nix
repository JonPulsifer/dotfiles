{
  bruh = "fortune | cowsay -f moose | lolcat";
  find = "fd --full-path";
  grep = "rg";
  fgrep = "rg";
  egrep = "rg";
  ll = "exa -lg";
  la = "exa -lag";
  ls = "exa";
  paths = "env | grep ^PATH= | cut -f2 -d= | tr -s : \\\\n | sort";
  switch = "home-manager switch";
  tree = "exa --tree";
  vim = "nvim";
}
