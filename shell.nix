let 
  pkgs = import <nixpkgs> {};

  cachixPkg = (fetchTarball https://codeload.github.com/NixOS/nixpkgs/legacy.tar.gz/a43702d670abf9b7e66d1e158617f01ecc074258);
  cachix = import cachixPkg {};

  ghcidePkg = (fetchTarball https://github.com/hercules-ci/ghcide-nix/tarball/master);
  ghcide = import ghcidePkg {};

  nixedCabal = pkgs.haskellPackages.callCabal2nix "dovlatov" ./. {};

in pkgs.stdenv.mkDerivation rec {
  name = "dovlatov";

  buildInputs = [
    pkgs.libiconv
    pkgs.zlib

    nixedCabal

    cachix.ghc
    cachix.cachix
    ghcide.ghcide-ghc865

    pkgs.ghcid
    pkgs.ghc
    pkgs.cabal-install
    pkgs.cabal2nix

    pkgs.haskellPackages.servant

    pkgs.figlet
  ];

  shellHook = ''
    export NIX_NAME='' + name + '' && figlet $NIX_NAME
  '';
}
