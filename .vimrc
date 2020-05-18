" Having trouble getting these to work; may be a vanilla Vim issue.
" Automatic reloading of .vimrc
" autocmd! bufwritepost .vimrc source %
" Update buffer when file is saved elsewhere
" set autoread

" Line numbers
set number relativenumber 

" https://vi.stackexchange.com/a/10125/25047
filetype plugin indent on 

" Figure out the correct indentation when creating a new line
set smartindent

" Convert tab to 2x spaces - work projects prefer standard tab indents
" set shiftwidth=2 expandtab

" Make file explorer default to tree view (i to cycle between views)
let g:netrw_liststyle = 3

" Make file explorer smaller by default
let g:netrw_preview   = 1
let g:netrw_winsize   = 15

" Color scheme using https://github.com/crusoexia/vim-monokai
" ~/.vim/colors/monokai.vim
colorscheme monokai
set background=dark

" Open vertical splits to right and horizontal splits below
set splitright splitbelow

" Enable syntax highlighting
syntax on

" For folding functions etc.
set foldmethod=indent
" https://vim.fandom.com/wiki/All_folds_open_when_opening_a_file
" set foldlevelstart=20

" Longer q: and undo histories
set history=1000
set undolevels=1000

" Content of yank goes to system clipboard
set clipboard=unnamed

" To prevent searching irrelevant directories when searching with **/ 
" https://www.reddit.com/r/vim/comments/7fzn9a/how_to_ignore_files_and_directories_from_edit/
set wildignore=*/.git/*,*/node_modules/*,*/dist/*,*/build/*,*/coverage/*

" To play nice with TypeScript
au BufReadPost *.tsx set syntax=javascript
au BufReadPost *.ts set syntax=javascript

" Git commit messsages
" From https://csswizardry.com/2017/03/configuring-git-and-vim/
" Break to new line at 72 characters
autocmd FileType gitcommit set textwidth=72
" Colour the 81st (or 73rd) column so that we don’t type over our limit
set colorcolumn=+1
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
call plug#end()

" Ale prettier autofix on save
" Not using this in curfent job.
" let g:ale_fix_on_save = 1
" let g:ale_fixers = {
" \   '*': ['prettier'],
" \}

" Open URL under cursor in Chrome when 'gx' is typed
" Note that for \"google-chrome\" on MacOS I use the alias 'open -a \"Google Chrome\"'
" Currently broken:
" https://github.com/vim/vim/issues/4738#issuecomment-612354457
" let g:netrw_browsex_viewer="google-chrome"
