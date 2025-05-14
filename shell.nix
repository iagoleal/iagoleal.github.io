let
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-24.11";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/NixOS/nixpkgs/archive/a39ed32a651fdee6842ec930761e31d1f242cb94.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "16pw0f94nr3j91z0wm4ndjm44xfd238vcdkg07s2l74znkaavnwk";
  }) {};
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
      pandoc_3_5
      lua5_4

      # Figures
      graphviz # 4.0.0
      scour    # 0.38.2
      tex
      ttfautohint
    ];

  LOCALE_ARCHIVE = /usr/lib/locale/locale-archive;
}
