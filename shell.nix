let
  pkgs = import <nixpkgs> {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-small
      dvisvgm
      luatex luatex85 standalone
      lm amsmath
      pgf tikz-cd;
  });
in
pkgs.mkShell {
  name = "site";

  packages = with pkgs; [
      # Workhorse
      gnumake
      pandoc   # 2.18
      lua5_4

      # Figures
      graphviz # 4.0.0
      scour    # 0.38.2
      tex
    ];

  LOCALE_ARCHIVE = /usr/lib/locale/locale-archive;
}
