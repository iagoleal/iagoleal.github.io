let
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-25.11";
    # Commit hash for nixos-unstable as of 2025-12-15
    url = "https://github.com/NixOS/nixpkgs/archive/c8cfcd6ccd422e41cc631a0b73ed4d5a925c393d.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1fdjh5jd5jx246fhfq13q5752nw9il5dwv36nqbcj4pa6kzwq9fy";
  }) {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng
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
      pandoc
      lua5_4

      # Figures
      graphviz # 4.0.0
      scour    # 0.38.2
      tex
      ttfautohint
      inkscape
    ];

  LOCALE_ARCHIVE = /usr/lib/locale/locale-archive;
}
