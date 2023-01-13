{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [
    cabal-install
    haskell.compiler.ghc925
    zlib.dev
  ];
}
