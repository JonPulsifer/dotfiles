self: super: {
  _1password = super._1password.overrideAttrs (oldAttrs: rec {
    version = "1.8.0";
    src = if super.stdenv.isLinux then
      super.fetchzip {
        url = {
          "i686-linux" =
            "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_386_v${version}.zip";
          "x86_64-linux" =
            "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_amd64_v${version}.zip";
          "aarch64-linux" =
            "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_arm_v${version}.zip";
        }.${super.stdenv.hostPlatform.system};
        sha256 = {
          "aarch64-linux" =
            "0932bspm1likky1n0rg15d01gspkm1fns2ma82qyb91yr6d18ddk";
          "i686-linux" = "teoxscan+EZ76Q0sfKT6nt1w/LSsmDoiN2oh+NGO/4A=";
          "x86_64-linux" = "nRK2GSwhQe5OgcAdR1fg0vUp3fzEkhwU/teIwsEEemw=";
        }.${super.stdenv.hostPlatform.system};
        stripRoot = false;
      }
    else
      super.fetchurl {
        url =
          "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_darwin_amd64_v${version}.pkg";
        sha256 = "0pycia75vdfh6gxfd2hr32cxrryfxydid804n0v76l2fpr9v9v3d";
      };
    meta = with super.lib; {
      platforms =
        [ "i686-linux" "x86_64-linux" "x86_64-darwin" "aarch64-linux" ];
    };
  });
}
