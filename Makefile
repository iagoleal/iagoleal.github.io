# Guarantee that things are properly defined
PYTHON_3 ?= python3
PANDOC ?= pandoc

# The important directories
content = content
cache   = cache
build   = build

# The important files
posts-src    := $(wildcard $(content)/posts/*)
posts-result := $(patsubst $(content)/posts/%.md.lhs,$(build)/posts/%/index.html,$(posts-src))
posts-result := $(patsubst $(content)/posts/%.md,$(build)/posts/%/index.html,$(posts-result))

# All additional pages go here
pages-names  = about masters posts
pages-result = $(addprefix $(build)/,$(addsuffix /index.html,$(pages-names)) \
                 index.html 404.html)

css-src    = $(wildcard css/*.css)
css-result = $(addprefix $(build)/,$(css-src))

static-src    = $(shell find static/ -type f)
static-result = $(patsubst %.tex,%.svg,$(addprefix $(build)/, $(static-src:static/%=%)))

# Dependency-only
filters   = $(wildcard filters/*)
templates = $(wildcard templates/*)
scripts   = $(wildcard scripts/*)

# Configuration files
config = pandoc.yaml

DEPENDENCIES = $(filters) $(templates) $(scripts) $(config)

#### Functions
define generate_page
  $(shell [ ! -d $(@D) ] && mkdir -p $(@D))
  $(PANDOC) --defaults=pandoc.yaml \
    -f $(3) -o "$(2)" "$(1)"
endef

define generate_post
  $(shell [ ! -d $(@D) ] && mkdir -p $(@D))
  $(PANDOC) --defaults=pandoc.yaml         \
            --shift-heading-level-by=1     \
            -M post                        \
    -f $(3) -o "$(2)" "$(1)"
endef

############
# Commands #
############

.DEFAULT: all

.PHONY: all clean serve watch clean-cache clean-build deploy

all: pages posts stylesheets static feed

clean: clean-cache clean-build

clean-cache:
	if [ -d $(cache) ]; then rm -rf $(cache); fi

clean-build:
	if [ -d $(build) ]; then rm -rf $(build); fi
	if [ -f 'pandoc-log.json' ]; then rm pandoc-log.json; fi

serve:
	cd $(build) && $(PYTHON_3) -m http.server

watch:
	watch -n 0.5 -- $(MAKE) all

deploy:
	scripts/deploy

#########
# Posts #
#########

.PHONY: posts

posts:   $(posts-result)

$(build)/posts/%/index.html: $(content)/posts/%.md $(DEPENDENCIES)
	$(call generate_post,"$<","$@",markdown)

$(build)/posts/%/index.html: $(content)/posts/%.md.lhs $(DEPENDENCIES)
	$(call generate_post,"$<","$@",markdown+lhs)

###############
# Other Pages #
###############

.PHONY: pages

pages: $(pages-result)

$(build)/index.html: $(content)/index.html $(DEPENDENCIES) $(posts-src)
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	$(PANDOC) --defaults=pandoc.yaml \
	   -f html -o "$@" "$<" \
	   -M title:'Home'

$(build)/404.html: $(content)/404.html   $(DEPENDENCIES)
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	$(PANDOC) --defaults=pandoc.yaml \
	   -f html -o "$@" "$<" \
	   -M title:'Are you lost?'

$(build)/%/index.html: $(content)/%.md   $(DEPENDENCIES)
	$(call generate_page,"$<","$@",markdown,'')

$(build)/%/index.html: $(content)/%.html $(DEPENDENCIES)
	$(call generate_page,"$<","$@",html,'')

###################
# Other processes #
###################

.PHONY: stylesheets static feed

feed: $(build)/rss.xml

$(build)/rss.xml: $(DEPENDENCIES)
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	$(PANDOC) --template=templates/rss.xml --lua-filter=filters/rss.lua \
	  --to plain --standalone \
	  -V site-url="https://iagoleal.com" -V author-meta="Iago Leal de Freitas" \
	  -f markdown -o "$@" /dev/null

stylesheets: $(css-result)

$(build)/css/%: css/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"
# hasmin -c "$<" > "$@"

static: $(static-result)

# Minimize svg files
$(build)/%.svg: static/%.svg
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	@echo Minifying file "$<"
	scripts/svg-minify "$<" "$@"

# Convert tikz images to svg
$(build)/%.svg: static/%.tex
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	scripts/tex2svg "$<" "$@"

# For {img,video,font,data}
$(build)/%: static/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"
