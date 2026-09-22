{
  pkgs ?
    import
      (fetchTarball {
        # Pinned because mdbook 0.5 broke mdbook-admonish's preprocessor
        # protocol (https://github.com/tommilligan/mdbook-admonish/issues/233),
        # and no fixed mdbook-admonish release exists yet. This revision has
        # mdbook 0.4.52, the last version compatible with mdbook-admonish 1.20.0.
        url = "https://github.com/NixOS/nixpkgs/archive/f575f2e29d4d27bfbe1887dca8f37611f1297cdf.tar.gz";
        sha256 = "1hh6r8zdp3arjdf4s7v82xvmn9zxdbkap0kb7yai65hd321bhss2";
      })
      { }
}:
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs =
    with pkgs;
    [
      mdbook
      mdbook-admonish
    ];
}
