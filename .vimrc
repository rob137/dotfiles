" Usually on by default, but just in case
set nocompatible

" https://vi.stackexchange.com/a/10125/25047
filetype plugin indent on 

" Syntax
" Enable highlighting
syntax on
" To play nice with TypeScript
au BufReadPost *.tsx set syntax=javascript
au BufReadPost *.ts set syntax=javascript

" Hide buffers instead of closing them - preserves undo history and allows you
" to switch buffers without saving
set hidden

" Fold
" For folding functions etc.
set foldmethod=indent
" Ensure all folds are unfolded when a file is opened
" set foldlevelstart=20

" Highlight current line of cursor
set cursorline

" Search
" Provides tab-completion for all file-related tasks
set wildmenu
" Display all matching files when we tab complete
set path+=**
" To prevent searching irrelevant directories when searching with **/ 
" https://www.reddit.com/r/vim/comments/7fzn9a/how_to_ignore_files_and_directories_from_edit/
set wildignore=*/.git/*,*/node_modules/*,*/dist/*,*/build/*,*/coverage/*
" Highlight search results, and jump to them as you type
set hlsearch incsearch
" Ignore case of search query unless a capital letter is used
set ignorecase smartcase

" Line numbers
set number relativenumber 
" Vertical column to show 80 chars
set colorcolumn=80

" Indentation
" Figure out the correct indentation when creating a new line
set autoindent
" Number of spaces used for indentation
set shiftwidth=2
" Convert tab to spaces - disabled since work projects prefer standard tab indents
" set expandtab
" Display tabs as 2 spaces wide
set tabstop=2
" Show tabs as characters (as opposed to whitespace, which is still used for spaces)
highlight SpecialKey ctermfg=1
set list
set listchars=tab:··

" File Explorer
" Make file explorer default to tree view (i to cycle between views)
let g:netrw_liststyle = 3
" Ensure file explorer preview pane is vertical and to right of screen
" (netrw_alto=0 is required to keep it on right while splitright is set)
let g:netrw_preview   = 1
let g:netrw_alto      = 0
" Make file explorer smaller by default
let g:netrw_winsize   = 15
" Prevent bug where netrw persists in buffer list even after :bdelete
" Doesn't seem to work...  This is a long-term issue with netrw. See:
" https://github.com/tpope/vim-vinegar/issues/13
let g:netrw_fastbrowse = 0

" Prevent window changing position when switching buffers
" From https://stackoverflow.com/a/4255960/8741502
if v:version >= 700
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif

"" -FORMER- Color scheme using https://github.com/crusoexia/vim-monokai
"" ~/.vim/colors/monokai.vim
" colorscheme monokai
" set background=dark

" Colorscheme
gruvbox colorscheme
autocmd vimenter * colorscheme gruvbox

" Open vertical splits to right and horizontal splits below
set splitright splitbelow

" Longer q: and undo histories
set history=1000 undolevels=1000

" Content of yank goes to system clipboard
set clipboard=unnamed

" Git commit messsages
" From https://csswizardry.com/2017/03/configuring-git-and-vim/
" Break to new line at 72 characters
autocmd FileType gitcommit set textwidth=72
" Also colour the 51st column (for titles)
autocmd FileType gitcommit set colorcolumn+=51

" This states plugins for Plug to install
" Requires separate install of plug from here:
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
Plug 'dense-analysis/ale',
Plug 'leafgarland/typescript-vim',
Plug 'peitalin/vim-jsx-typescript'
Plug 'cakebaker/scss-syntax.vim'
Plug 'morhetz/gruvbox'
call plug#end()

" Autofix with Ale 
" I've previously just used '*': ['prettier'] and left it at that.
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   'css': ['prettier'],
\   'less': ['prettier'],
\   'json': ['eslint'],
\   'javascript': ['eslint'],
\}

" $HOME/.vimrc
" must first run 'mkdir ~/.vim/swapfiles'
:set directory=$HOME/.vim/swapfiles//

" Allows language-specific autocompletion in insert mode with CTRL-X CTRL-O
set omnifunc=syntaxcomplete#Complete

" Settings I would like to use, but can't get working:

" Open URL under cursor in Chrome when 'gx' is typed
" Note that for \"google-chrome\" on MacOS I use the alias 'open -a \"Google Chrome\"'
" Currently broken:
" https://github.com/vim/vim/issues/4738#issuecomment-612354457
" let g:netrw_browsex_viewer="google-chrome"

" Appears to be a vanilla Vim issue - these work in gvim / neovim, but not vim.
" Automatic reloading of .vimrc
" autocmd! bufwritepost .vimrc source %
" Update buffer when file is saved elsewhere
" set autoread

