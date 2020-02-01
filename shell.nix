let 
  pkgs = import <nixpkgs> {};

in pkgs.stdenv.mkDerivation rec {
  name = "dovlatov";

  buildInputs = [

    pkgs.libiconv
    pkgs.zlib

    pkgs.ghc
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.cabal2nix

    pkgs.figlet
  ];

  shellHook = ''
    export NIX_NAME='' + name + '' && figlet $NIX_NAME
  '';
}
