runtime syntax/haskell.vim

hi def link agdaNumber           Number
hi def link agdaString           String
hi def link agdaConstructor      Constant
hi def link agdaCharCode         SpecialChar
hi def link agdaCharCodeErr      Error
hi def link agdaHole             WarningMsg
hi def link agdaDubious          WarningMsg
hi def link agdaKeywords         Structure
hi def link agdaFunction         Macro
hi def link agdaOperator         Operator
hi def link agdaInfixConstructor Operator
hi def link agdaInfixFunction    Operator
hi def link agdaLineComment      Comment
hi def link agdaBlockComment     Comment
hi def link agdaPragma           Comment
hi def      agdaTODO             cterm=bold,underline ctermfg=2 " green
hi def      agdaFIXME            cterm=bold,underline ctermfg=3 " yellow
hi def      agdaXXX              cterm=bold,underline ctermfg=1 " red
