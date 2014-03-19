all: install 

install: submodules xmonad
	rsync --exclude ".git" \
				--exclude ".gitmodules" \
				--exclude ".*sw*" \
				--exclude "Makefile"\
				-avzb . ~

submodules:
	git submodule init
	git submodule update

xmonad:
	cd .xmonad && ghc -ilib xmonad.hs && cd ..
