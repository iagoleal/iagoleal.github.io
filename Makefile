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

# For yet unpublished posts
unposts-src    = $(wildcard $(content)/unposts/*.md)
unposts-result = $(patsubst $(content)/unposts/%.md,$(build)/posts/%/index.html,$(unposts-src))

# All additional pages go here
pages-names  = about masters posts
pages-result = $(addprefix $(build)/,$(addsuffix /index.html,$(pages-names)) \
                 index.html 404.html)
css-src    = $(wildcard css/*.css)
css-result = $(addprefix $(build)/,$(css-src))

static-src    = $(wildcard static/**/*)
static-result = $(patsubst %.tex,%.svg,$(addprefix $(build)/, $(static-src:static/%=%)))

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

.PHONY: all clean serve watch clean-cache clean-build deploy

all: pages posts stylesheets static

unpublished: all unposts

clean: clean-cache clean-build

clean-cache:
	if [ -d $(cache) ]; then rm -rf $(cache); fi

clean-build:
	if [ -d $(build) ]; then rm -rf $(build); fi
	if [ -f 'pandoc-log.json' ]; then rm pandoc-log.json; fi

serve:
	cd $(build) && $(PYTHON_3) -m http.server

watch:
	cd $(build) && $(PYTHON_3) -m http.server &
	watch -n 0.5 $(MAKE)

deploy:
	sh deploy

#########
# Posts #
#########

.PHONY: posts unposts

posts: $(posts-result)
unposts: $(unposts-result)

$(build)/posts/%/index.html: $(content)/posts/%.md $(filters) $(templates) $(config)
	$(call generate_post,"$<","$@")

$(build)/posts/%/index.html: $(content)/unposts/%.md $(filters) $(templates) $(config)
	$(call generate_post,"$<","$@")

###############
# Other Pages #
###############

.PHONY: pages

pages: $(pages-result)

$(build)/index.html: $(content)/index.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html,"Home Sweet Home")

$(build)/404.html: $(content)/404.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html,"Are you lost?")

$(build)/%/index.html: $(content)/%.md $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",markdown,'')

$(build)/%/index.html: $(content)/%.html $(filters) $(templates) $(config)
	$(call generate_page,"$<","$@",html,'')

###################
# Other processes #
###################

.PHONY: stylesheets static

stylesheets: $(css-result)

$(build)/css/%: css/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"
# hasmin -c "$<" > "$@"

static: $(static-result)

# Convert tikz images to svg
$(build)/img/%.svg: static/img/%.tex
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	src/tex2svg.sh "$<" "$@"

# For {img,video,font,data}
$(build)/%: static/%
	$(shell [ ! -d $(@D) ] && mkdir -p $(@D))
	cp "$<" "$@"
