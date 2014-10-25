call pathogen#infect()

" Section: Options {{{


" Environment
set nocompatible
set shell=bash

" Visual
set noshowmode
set wrap
set title
set ls=0
set shortmess+=I
set report=9999999999999999999999999999999999999999999

if &t_Co > 2 || has("gui_running")
  syntax enable
  colorscheme industry
endif

if &t_Co >= 256 || has("gui_running")
  colorscheme solarized
  if system('echo -n $TERM')=="rxvt-unicode-256color" || system("echo -n $TERM")=="screen-256color"
    set background=dark
  else
    let g:solarized_termtrans = 1
    set background=light
  endif
endif
  
highlight NonText ctermfg=8 ctermbg=8

" Title
function! Chomp(string)
  return substitute(a:string, "\n*$", "", "")
endfunction

function! Run(command)
  return Chomp(system(a:command))
endfunction

function! Git(cmd)
  let gd = b:git_dir
  let wt = substitute(b:git_dir, ".git$", "", "")
  let pr = "git --git-dir=" . gd . " --work-tree=" . wt . " "
  return Run(pr . a:cmd)
endfunction

function! GitTitle(...) 
  if !exists('b:git_dir')
    return ''
  endif
  let branch  = Git("branch | awk '/\\*/ {print $2}'")
  let changes = Git("status -s | wc -l | sed 's/^ \*/:/'")
  return "[" . branch . changes . "]"
endfunction

function! NixTitle(...)
  if $IN_NIX_SHELL == '1'
    return '[nix-shell]'
  endif
  return ''
endfunction

function! DirTitle(...)
  let dir = substitute(getcwd(),$HOME,'~','')
  return dir . " "
endfunction

if has('title') && (has('gui_running') || &title)
  set titlestring=%{DirTitle()}
  set titlestring+=%{GitTitle()}
  set titlestring+=%{NixTitle()}
endif

if has('gui_running')
  set guifont=LetterGothicMono-Light:h16
  set guicursor+=a:blinkon0
  set guioptions=
endif

" Indentation
set autoindent
set copyindent
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set formatoptions=t1

" Backups
set backup
set backupdir=~/.vim/backup
set writebackup

" Window Splitting
set splitbelow
set splitright

" Code Folding
set foldenable
set foldmethod=marker
set foldmarker={{{,}}}
set fillchars=
set foldcolumn=0

" Insert Behaviours
set backspace=start

" Document Navigation/Search
set incsearch

" Buffer Navigation Behaviours
set autowrite
set hidden
if has("autocmd")
  " Restore cursor position after reopening files.
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" Errors
set noerrorbells
set visualbell t_vb=
if has("autocmd")
  autocmd GUIEnter * set visualbell t_vb=
endif

" FileType
filetype plugin on
set wildignore='.git'
if has("autocmd")
  autocmd FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
  autocmd FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
endif

set autowrite
set autoread

set mouse=a
set ffs=unix,dos,mac

" }}}
" Section: Commands {{{

" Toggle the QuickFix Window.
command! -bang -nargs=? QFix call QFixToggle(<bang>0)
function! QFixToggle(forced)
  if exists("g:qfix_win") && a:forced == 0
    cclose
    unlet g:qfix_win
  else
    copen 10
    let g:qfix_win = bufnr("$")
  endif
endfunction

function! ToggleSyntax()
  if exists("g:syntax_on")
    syntax off
  else
    syntax on
  endif
endfunction

command! RTime :r! date "+\%F \%T \%z"
command! Paste :! curl -F 'sprunge=<-' http://sprunge.us <%

" }}}
" Section: Mappings {{{

" Command Triggers
let mapleader=","
map ;  :


" File Navigation
nnoremap <leader>m  /TODO<CR>
nnoremap <leader>x  /TODO<CR>
nnoremap <Space>    Lz<CR>

" Buffer Navigation
nnoremap <leader>b :ls<CR>:b<space>
nnoremap <leader>d :ls<CR>:bdelete<space>

" Reset
nmap <C-h> <C-w>h<CR><CR>
nmap <C-j> <C-w>j<CR><CR>
nmap <C-k> <C-w>k<CR><CR>
nmap <C-l> <C-w>l<CR><CR>

" Search
cmap g!! vimgrep
nnoremap <leader>f :find<space>

" Block Commenting
noremap <leader># :s/^/#/<CR>
noremap <leader>/ :s`^`//`<CR>
noremap <leader>! :s/^/!/<CR>
noremap <leader>- :s/^/--/<CR>
noremap <leader>" :s/^/"/<CR>
noremap <leader>' :s/^/'''/<CR>

" Mode Toggling
nmap <silent><leader>l :set rnu!<CR>
nmap <silent><leader>h :call ToggleSyntax()<CR>

nmap <silent><leader>p :set paste!<CR>


" User Functions
nmap <silent><leader>w :call StripWhitespace()<CR>
nmap <silent><leader>q :QFix()<CR>

" Miscellaneous
nmap <silent><leader>s :shell<CR>
nmap <Leader>m :make<CR>
cmap w!! w !sudo tee % >/dev/null
noremap <leader>w :%s/ \+$//ge<CR>
map <Leader>ev :vs $MYVIMRC<CR>

" }}}
