{ nixpkgs ? import <nixpkgs> { } }:
nixpkgs.stdenv.mkDerivation {
  pname = "shell-utils";
  version = "0.1.0";
  src = ./bin;
  buildinputs = [ nixpkgs.bash ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src/* $out/bin
  '';
}
