let
  pkgs = import <nixpkgs> {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-medium
      dvisvgm amsmath
      standalone forest tikz-cd;
  });
in
pkgs.stdenv.mkDerivation {
  name = "site";

  buildInputs = with pkgs; [
      # Workhorse
      gnumake
      pandoc   # 2.18
      lua5_4

      # Figures
      graphviz # 4.0.0
      scour    # 0.38.2
      tex
    ];
}
