let
  pkgs = import <nixpkgs> {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm
      luatex luatex85 standalone
      lm amsmath amsfonts xcolor etoolbox
      pgf tikz-cd;
  });
in
pkgs.mkShell {
  name = "site";

  packages = with pkgs; [
      # Workhorse
      gnumake
      pandoc_3_6
      lua5_4

      # Figures
      graphviz # 4.0.0
      scour    # 0.38.2
      tex
    ];

  LOCALE_ARCHIVE = /usr/lib/locale/locale-archive;
}
