# Personal site's source code

My own [personal website](https://iagoleal.com/).
There is also a [RSS feed](https://iagoleal.com/rss.xml) for the posts in case you're interested.

The site theme is built upon oxalorg's
[sakura-earthly](https://github.com/oxalorg/sakura) theme.

The font in use is [Montserrat](https://github.com/JulietaUla/Montserrat).

The math is rendered using [katex](https://katex.org/).

Build should be as simple as running

```
$ make site
```

Deployment to GitHub Pages happens automatically on push to `master`.


## Features and Non-features

### Social Media sharing

I may not be the biggest fan of social media, but I know it is here to stay.
Hence, it is useful to keep in mind how my pages interact with any of these sites.

Sharing a post should show its title, its description,
and a general (site-wide) banner.
We achieve this via the (Open Graph protocol)[https://ogp.me/].
For customizing any tags,
edit the file <templates/open-graph.html>.

Every post also ends with buttons for easily sharing it
on some instant message apps or social media sites.
You can find them in the file <templates/post-share.html>.
