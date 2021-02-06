{ ddnsDomain ? "home.pulsifer.ca"
, ddnsEndpoint ? "https://us-east4-homelab-ng.cloudfunctions.net/ddns"
, buildGoModule, fetchFromGitHub, lib, makeWrapper }:

buildGoModule rec {
  pname = "ddnsb0t";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "jonpulsifer";
    repo = "ddnsb0t";
    rev = "b46f5588669366044abadc910539956a007a0420";
    sha256 = "0xpivvskrgq75b2zpv0103bb82pa1cqzs6l2af0khb11pjp87sfc";
  };
  vendorSha256 = "1m5l95ipzk27gcpnhfdl2d2h2816679b19w3c3qsrzj28bj1v7i8";
  subPackages = [ "." ];

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/ddnsb0t \
      --add-flags "--once" \
      --set DDNS_DOMAIN ${ddnsDomain} \
      --set DDNS_ENDPOINT ${ddnsEndpoint}
  '';

  meta = with lib; {
    description =
      "ddnsb0t is a program that uses CloudEvents to communicate to a Google Cloud Function and update my DNS entries using Google Cloud DNS.";
    homepage = "https://github.com/jonpulsifer/ddnsb0t";
    license = licenses.mit;
    maintainers = with maintainers; [ jonpulsifer ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
