let 
  pkgs = import <nixpkgs> {};

  cachixPkg = (fetchTarball https://codeload.github.com/NixOS/nixpkgs/legacy.tar.gz/a43702d670abf9b7e66d1e158617f01ecc074258);
  cachix = import cachixPkg {};

  ghcidePkg = (fetchTarball https://github.com/hercules-ci/ghcide-nix/tarball/master);
  ghcide = import ghcidePkg {};

  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "45f5db65fc2453e757c60ae54c611d1d8baa20cf";
    sha256 = "1gc3p5xivb2k9jm22anzm6xy1cnzw2ab6jq8ifws92pvfnvx0lxv";
  }) { inherit pkgs; };

  esyPkg = import (pkgs.fetchFromGitHub {
    owner = "anmonteiro";
    repo = "esy";
    rev = "7fed16bbd90d472710b3ef51284e199b2be7c279";
    sha256 = "1p8pmlx5vcj70rbrvkijmh268xwn6a5asp5drplqhiszvs6pfp6m";
  }) { inherit pkgs; };


in pkgs.stdenv.mkDerivation rec {
  name = "dovlatov";

  buildInputs = [

    pkgs.libiconv
    pkgs.zlib

    esyPkg.esy

    pkgs.elmPackages.elm

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
