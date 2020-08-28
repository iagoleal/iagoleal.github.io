PYTHON_3?=python3

BUILDDIR = _site
POSTSDIR = posts
CSSDIR   = css
POSTS= $(addprefix $(BUILDDIR)/, $(patsubst %.md,%.html, $(wildcard $(POSTSDIR)/*.md)))
FILTERS = $(wildcard filters/*.lua)
DIRS = $(addprefix $(BUILDDIR)/, $(POSTSDIR))

.DEFAULT: all
.PHONY: all clean server filters

$(POSTS): $(BUILDDIR)/%.html : %.md $(FILTERS) | $(DIRS)
	pandoc --lua-filter filters/tikz.lua --self-contained -s -f markdown -t html5 -o "$@" "$<"


all: $(POSTS) $(DIRS)

$(DIRS):
	mkdir -p "$@"

# Preview site
server: ## Preview site
	cd $(BUILDDIR) && $(PYTHON_3) -m http.server

clean: ## Delete cache and build files
	rm -r $(BUILDDIR)
	rm -r _cache


help:
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed -n 's/^\(.*\): \(.*\)##\(.*\)/\1\3/p' \
	| column -t  -s ' '
