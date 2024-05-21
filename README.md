# Personal Site's Source Code

My own [personal website](https://iagoleal.com/).
There is also an [RSS feed](https://iagoleal.com/rss.xml) for the posts---in case you're interested.

The site theme is built upon oxalorg's
[sakura-earthly](https://github.com/oxalorg/sakura) theme.

The font in use is [Montserrat](https://github.com/JulietaUla/Montserrat).

Build should be as simple as running

```
$ make site
```

Deployment to GitHub Pages happens automatically on push to `master`.

## Features and Non-features

### TeX Notation for Math

This site uses [katex](katex.org) to render equations.
Anything between `$...$` or `$$...$$` is interpreted as mathematics
and properly converted.

### Compile LaTeX to SVG

Files in the <static/> folder with extension `.tex`
are interpreted as images (usually used for TiKZ graphics),
thus we can compile them to SVG.

If there is both `path/to/bla.svg` and `path/to/bla.tex`,
the former has priority.
Eventually, I shall turn it into an error.

### Compile Code Blocks to Figures

Some special Markdown code blocks are converted into their output figure
during compilation to HTML.
The external dependencies for the backends are already pulled with Nix.
Right now, the following environments are supported:

- `tikz`
- `tikzcd`
- `dot` (graphviz)

Some of these environments also accept additional configurations
via the code block's attributes.
Check the code for each of them at <filters/figure.lua>
to check the accepted attributes.


To add a new backend, edit the Pandoc filter at <filters/figure.lua>
and add the necessary dependencies to <shell.nix>.

### Social Media sharing

I may not be the biggest fan of social media, but I know it is here to stay.
Hence, it is useful to keep in mind how my pages interact with any of these sites.

Sharing a post should show its title, its description,
and a general (site-wide) banner.
We achieve this via the [Open Graph protocol](https://ogp.me/).
For customizing any tags,
edit the file <templates/open-graph.html>.

Every post also ends with buttons for easily sharing it
on some instant message apps or social media sites.
You can find them in the file <templates/post-share.html>.
