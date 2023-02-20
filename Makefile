.PHONY: all dotfiles

all: dotfiles

dotfiles:
	# add aliases
	for file in $(shell find $(CURDIR) -maxdepth 1 -name ".*" -not -name ".gitignore" -not -name ".git" -not -name "*~"); do \
		f=$$(basename $$file); \
		ln -sfn $$file $(HOME)/$$f; \
	done;
