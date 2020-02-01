let 
  pkgs = import <nixpkgs> {};

in pkgs.stdenv.mkDerivation rec {
  name = "vaba";

  buildInputs = [

    pkgs.libiconv
    pkgs.zlib

    pkgs.ghc
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.cabal2nix

    pkgs.elmPackages.elm

    pkgs.figlet
  ];

  shellHook = ''
    export NIX_NAME='' + name + '' && figlet $NIX_NAME
  '';
}
