all: install 

install:
	rsync --exclude ".git" \
				--exclude ".gitignore" \
				--exclude ".gitmodules" \
				--exclude "README.md" \
				--exclude "README.pdf"\
				--exclude ".*sw*" \
				--exclude "Makefile"\
				-avzb . ~

submodules:
	git submodule init
	git submodule update
