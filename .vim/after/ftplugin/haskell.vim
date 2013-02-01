silent! compiler ghc

let g:haddock_browser      = $BROWSER
let g:haddock_indexfiledir = "~/.vim"

setlocal shiftwidth=4

map <Leader>g :!ghci %<CR>
