# Personal site's source code

My own [personal website](https://iagoleal.com/).
There is also a [RSS feed](https://iagoleal.com/rss.xml) for the posts in case you're interested.

The site theme is built upon oxalorg's
[sakura-earthly](https://github.com/oxalorg/sakura) theme.

The font in use is [Montserrat](https://github.com/JulietaUla/Montserrat).

The math is rendered using [katex](https://katex.org/).

Building and deploying should be as simple as running

```
$ make site && make deploy
```

## Dependencies
- pandoc 2.9.2.1
- make 4.3
- latex (texlive pdftex 3.14) (including tikz)
- dvisvgm 2.9.1
