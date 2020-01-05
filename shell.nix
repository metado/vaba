let 
  pkgs = import <nixpkgs> {};

  cachixPkg = (fetchTarball https://codeload.github.com/NixOS/nixpkgs/legacy.tar.gz/a43702d670abf9b7e66d1e158617f01ecc074258);
  cachix = import cachixPkg {};

  ghcidePkg = (fetchTarball https://github.com/hercules-ci/ghcide-nix/tarball/master);
  ghcide = import ghcidePkg {};

in pkgs.stdenv.mkDerivation rec {
  name = "dovlatov";

  buildInputs = [
    pkgs.libiconv
    pkgs.zlib

    cachix.ghc
    cachix.cachix
    ghcide.ghcide-ghc865

    pkgs.ghcid
    cachix.cabal-install
    cachix.cabal2nix

    cachix.haskellPackages.servant
    cachix.haskellPackages.aeson

    pkgs.figlet
  ];

  shellHook = ''
    export NIX_NAME='' + name + '' && figlet $NIX_NAME
  '';
}
