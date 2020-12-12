# Guarantee that things are properly defined
PYTHON_3 ?= python3
PANDOC ?= pandoc

# The important directories
content = content
cache   = cache
build   = build

# The important files
posts-src    = $(wildcard $(content)/posts/*.md)
posts-result = $(patsubst $(content)/posts/%.md,$(build)/posts/%/index.html,$(posts-src))

# All additional pages go here
pages-names  = about projects posts
pages-result = $(addprefix $(build)/,$(addsuffix /index.html,$(pages-names)) \
                 index.html 404.html)

css-src    = $(wildcard css/*.css)
css-result = $(addprefix $(build)/,$(css-src))

static-src    = $(wildcard img/*) $(wildcard data/*)
static-result = $(addprefix $(build)/, $(static-src))

# Dependency-only
filters   = $(wildcard filters/*)
templates = $(wildcard templates/*)

# Configuration files
config = pandoc.yaml

#### Functions
define generate_page
  $(shell [ ! -d $(@D) ] && mkdir -p $(@D))
  $(PANDOC) --defaults=pandoc.yaml \
    -f $(3) -t html5 -o "$(2)" "$(1)"
endef

define generate_post
  $(shell [ ! -d $(@D) ] && mkdir -p $(@D))
  $(PANDOC) --defaults=pandoc.yaml \
            --template=templates/post.html \
    -f markdown -t html5 -o "$(2)" "$(1)"
endef

############
# Commands #
############

.DEFAULT: all

.PHONY: all clean serve clean-cache clean-build

all: pages posts stylesheets static

clean: clean-cache clean-build

clean-cache:
	if [ -d $(cache) ]; then rm -rf $(cache); fi

clean-build:
	if [ -d $(build) ]; then rm -rf $(build); fi
	if [ -f 'pandoc-log.json' ]; then rm pandoc-log.json; fi

serve:
	pushd $(build) && $(PYTHON_3) -m http.server

#########
# Posts #
#########

.PHONY: posts

posts: $(posts-result)

$(build)/posts/%/index.html: $(content)/posts/%.md $(filters) $(templates) $(config)
	$(call generate_post,"$<","$@")

###############
# Other Pages #
###############

.PHONY: pages

pages: $(pages-result)

$(build)/index.html: $(content)/index.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html)

$(build)/404.html: $(content)/404.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html)

$(build)/%/index.html: $(content)/%.md $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",markdown)

$(build)/%/index.html: $(content)/%.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html)

###################
# Other processes #
###################

.PHONY: stylesheets static

stylesheets: $(css-result)

$(build)/css/%: css/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cat "$<" | tr -d '\n' | perl -0777 -pe 's{/\*.*?\*/}{}gs' \
	| sed 's/\([{}:;,>]\)\s\+/\1/g' \
	| sed 's/\s\+\([{},:;]\)/\1/g' > "$@"

static: $(static-result)

$(build)/img/%: img/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"

$(build)/data/%: data/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"
