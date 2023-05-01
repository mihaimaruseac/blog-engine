{ pkgs ? import <nixpkgs> {} }:

let
  localGHC = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    diagrams
    diagrams-rasterific
    palette
  ]);
in
  with pkgs;
  mkShell {
    buildInputs = [
      localGHC
      cabal-install
      zlib.dev
    ];

    shellHook = ''
      export PS1="[\[\033[01;32m\]nix-shell\[\033[00m\]:\W] \[\033[01;32m\]λ\[\033[00m\] "
    '';
  }
