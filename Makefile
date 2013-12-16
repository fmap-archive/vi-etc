all: install 

install:
	rsync --exclude ".git" \
				--exclude ".gitmodules" \
				--exclude ".*sw*" \
				--exclude "Makefile"\
				-avzb . ~

submodules:
	git submodule init
	git submodule update
