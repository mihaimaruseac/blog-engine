{ pkgs ? import <nixpkgs> {} }:

let
  localGHC = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    diagrams
    palette
    # diagrams-html5
    # diagrams-svg
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
