{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs =
    with pkgs;
    [
      mdbook
      mdbook-admonish
    ];
}
