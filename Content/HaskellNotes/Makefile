# Edited From: https://gist.github.com/bertvv/e77e3a5d24d8c2a9bcc4

# Generate PDFs from the Markdown source files
#
# In order to use this makefile, you need some tools:
# - GNU make
# - Pandoc
# - LuaLaTeX

# Directory containing source (Markdown) files
source := markdown

# Directory containing pdf files
output := pdf

# All markdown files in src/ are considered sources
sources := $(wildcard $(source)/*.md)

# Convert the list of source files (Markdown files in directory src/)
# into a list of output files (PDFs in directory print/).
objects := $(patsubst %.md,%.pdf,$(subst $(source),$(output),$(sources)))

all: $(objects)

# Recipe for converting a Markdown file into PDF using Pandoc
#$(output)/%.pdf: pandoc $(source)/%.md --pdf-engine=latexmk -o $@
$(output)/%.pdf: $(source)/%.md
	pandoc \
		--variable fontsize=11pt \
		--variable geometry:"top=1.5cm, bottom=2.5cm, left=1.5cm, right=1.5cm" \
		--variable geometry:a4paper \
		-f markdown  $< \
  	--pdf-engine=latexmk \
  	-o $@

# 		-f markdown  $< \
# 		--latex-engine=lualatex \
# 		-o $@

.PHONY : clean

clean:
	rm -f $(output)/*.pdf