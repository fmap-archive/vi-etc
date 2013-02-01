if exists("current_compiler")
  finish
endif
let current_compiler = "vbnc"

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=vbnc\ -nologo\ -quiet\ %
autocmd BufNewFile Compilation* bdelete
