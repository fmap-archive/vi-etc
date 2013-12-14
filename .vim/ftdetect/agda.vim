au BufNewFile,BufRead *.agda set ft=agda
au BufNewFile,BufRead *.agda call WriteReadSyntax()
au FileWritePost,BufWritePost *.agda call WriteReadSyntax()
