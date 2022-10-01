let
  pkgs = import <nixpkgs> {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-medium
      dvisvgm amsmath
      standalone forest tikz-cd;
  });
in
pkgs.stdenv.mkDerivation {
  name = "my-env";

  buildInputs =
    [
      # Workhorse
      pkgs.gnumake
      pkgs.pandoc
      pkgs.lua5_4

      # Figures
      pkgs.graphviz
      pkgs.scour
      tex
    ];
}
