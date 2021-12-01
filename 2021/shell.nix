let
  # pin the upstream nixpkgs
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/98747f27ecfee70c8c97b195cbb94df80a074dda.tar.gz";
    sha256 = "04ss525ns5qqlggrdhvc6y4hqmshylda9yd0y99ddliyn15wmf27";
  };
  nixpkgsSrc = (import nixpkgsPath);
  pkgs = nixpkgsSrc { };

  gleam = pkgs.rustPlatform.buildRustPackage rec {
    pname = "gleam";
    version = "0.18.0-rc1";

    src = pkgs.fetchFromGitHub {
      owner = "gleam-lang";
      repo = pname;
      rev = "v${version}";
      sha256 = "sha256-STktCpL3sRDNP6JjZ0Un8yVawGYAQdZ1VJ56S9eZeDc=";
    };

    nativeBuildInputs = [ pkgs.pkg-config ];

    buildInputs = [ pkgs.openssl ];

    cargoSha256 = "sha256-MBUnFiaGEgEnFLyKDkommeTwuepmj1uBsAtljQRyp/o=";

    meta = with pkgs.lib; {
      description = "A statically typed language for the Erlang VM";
      homepage = "https://gleam.run/";
      license = licenses.asl20;
      maintainers = teams.beam.members;
    };
  };

in pkgs.mkShell { buildInputs = [ gleam pkgs.erlang_nox ]; }
