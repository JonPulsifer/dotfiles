# https://github.com/NixOS/nixpkgs/issues/105119
self: super: {
  nmap = super.nmap.overrideAttrs (oldAttrs: rec {
    version = "7.91";
    src = super.fetchurl {
      url = "https://nmap.org/dist/nmap-${version}.tar.bz2";
      sha256 = "001kb5xadqswyw966k2lqi6jr6zz605jpp9w4kmm272if184pk0q";
    };
  });
}
