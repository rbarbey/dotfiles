.PHONY: all dotfiles

all: dotfiles

dotfiles:
	# add aliases
	for file in $(shell find $(CURDIR) -type f -name ".*" -not -name "*.gitignore"); do \
		f=$$(basename $$file); \
		ln -sfn $$file $(HOME)/$$f; \
	done;
