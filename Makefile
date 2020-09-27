PYTHON_3?=python3

BUILDDIR = build
POSTSDIR = posts
CACHEDIR = cache
POSTS= $(addprefix $(BUILDDIR)/, $(patsubst %.md,%, $(wildcard $(POSTSDIR)/*.md)))
CSS = $(addprefix $(BUILDDIR)/, $(wildcard css/*.css))
FILTERS = $(wildcard filters/*.lua)
DIRS = $(addprefix $(BUILDDIR)/, $(POSTSDIR) css data img) $(CACHEDIR)
TEMPLATES = $(wildcard templates/*)
STATIC_FILES = $(addprefix $(BUILDDIR)/, $(wildcard data/*) $(wildcard img/*))


.DEFAULT: all
.PHONY: all clean server pages postspage projectspage

all: pages $(DIRS)

pages: postspage projectspage $(BUILDDIR)/index.html $(DIRS) $(POSTS) $(STATIC_FILES) pandoc.yaml

projectspage: $(BUILDDIR)/projects/index.html

postspage: $(BUILDDIR)/posts/index.html

$(POSTS): $(BUILDDIR)/% : %.md $(FILTERS) $(CSS) $(DIRS) $(TEMPLATES)
	mkdir -p "$@"
	pandoc --defaults=pandoc.yaml \
	       --lua-filter filters/tikz.lua \
	       --lua-filter filters/wc.lua \
               -f markdown -t html5 -o "$@/index.html" "$<"

$(BUILDDIR)/index.html: index.html $(CSS) $(DIRS) $(TEMPLATES)
	pandoc --defaults=pandoc.yaml \
		--metadata=title:'Iago Leal' \
               -f html -t html5 -o "$@" "$<"

$(BUILDDIR)/projects/index.html: projects.md $(CSS) $(DIRS) $(TEMPLATES)
	mkdir -p "$(BUILDDIR)/projects"
	pandoc --defaults=pandoc.yaml \
               -f markdown -t html5 -o "$@" "$<"

$(BUILDDIR)/posts/index.html: posts.md $(CSS) $(DIRS) $(TEMPLATES)
	mkdir -p "$(BUILDDIR)/posts"
	pandoc --defaults=pandoc.yaml \
               -f markdown -t html5 -o "$@" "$<"

# Order of minifying:
# - Remove all newlines
# - Remove all spaces after ':', ';', '{', ',', '>'
# - Remove all spaces before '{'
$(CSS): $(BUILDDIR)/css/%.css : css/%.css $(DIRS)
	cat "$<" | tr -d '\n' | sed 's/\([{:;,>]\)\s\+/\1/' | sed 's/\s\+\({\)/\1/' > "$@"

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
