PYTHON_3?=python3

BUILDDIR = build
POSTSDIR = posts
CACHEDIR = cache
POSTS= $(addprefix $(BUILDDIR)/, $(patsubst %.md,%.html, $(wildcard $(POSTSDIR)/*.md)))
CSS = $(addprefix $(BUILDDIR)/, $(wildcard css/*.css))
FILTERS = $(wildcard filters/*.lua)
DIRS = $(addprefix $(BUILDDIR)/, $(POSTSDIR) css data img) $(CACHEDIR)
TEMPLATES = $(wildcard templates/*)
STATIC_FILES = $(addprefix $(BUILDDIR)/, $(wildcard data/*) $(wildcard img/*))


.DEFAULT: all
.PHONY: all clean server pages

all: pages $(DIRS)

pages: $(BUILDDIR)/posts.html $(BUILDDIR)/index.html $(BUILDDIR)/projects.html $(DIRS) $(POSTS) $(STATIC_FILES) pandoc.yaml

$(POSTS): $(BUILDDIR)/%.html : %.md $(FILTERS) $(CSS) $(DIRS) $(TEMPLATES)
	pandoc --defaults=pandoc.yaml \
	       --lua-filter filters/tikz.lua \
               -f markdown -t html5 -o "$@" "$<"

$(BUILDDIR)/index.html: index.html $(CSS) $(DIRS) $(TEMPLATES)
	pandoc --defaults=pandoc.yaml \
		--metadata=title:'Iago Leal' \
               -f html -t html5 -o "$@" "$<"

$(BUILDDIR)/projects.html: projects.md $(CSS) $(DIRS) $(TEMPLATES)
	pandoc --defaults=pandoc.yaml \
               -f markdown -t html5 -o "$@" "$<"

$(BUILDDIR)/posts.html: posts.md $(CSS) $(DIRS) $(TEMPLATES)
	pandoc --defaults=pandoc.yaml \
               -f markdown -t html5 -o "$@" "$<"

$(CSS): $(BUILDDIR)/css/%.css : css/%.css $(DIRS)
	cp "$<" "$@"

$(STATIC_FILES): $(BUILDDIR)/% : % $(DIRS)
	cp "$<" "$@"

$(DIRS):
	mkdir -p "$@"

# Preview site
server: ## Preview site
	cd $(BUILDDIR) && $(PYTHON_3) -m http.server

clean: ## Delete cache and build files
	if [ -d $(BUILDDIR) ]; then rm -r $(BUILDDIR); fi
	if [ -d $(CACHEDIR) ]; then rm -r $(CACHEDIR); fi
	if [ -f 'pandoc-log.json' ]; then rm pandoc-log.json; fi
