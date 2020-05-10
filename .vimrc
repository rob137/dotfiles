" Make file explorer smaller by default
let g:netrw_preview   = 1
let g:netrw_winsize   = 15

" Color scheme using https://github.com/crusoexia/vim-monokai
" ~/.vim/colors/monokai.vim
colorscheme monokai
set background=dark

syntax on
set foldmethod=syntax
" https://vim.fandom.com/wiki/All_folds_open_when_opening_a_file
set foldlevelstart=20

" Longer :q command history
set history=1000

" To prevent searching irrelevant directories when searching with **/ 
" https://www.reddit.com/r/vim/comments/7fzn9a/how_to_ignore_files_and_directories_from_edit/
set wildignore=*/.git/*,*/node_modules/*,*/dist/*,*/build/*

" To play nice with TypeScript
au BufReadPost *.tsx set syntax=javascript
au BufReadPost *.ts set syntax=javascript

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
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['prettier'],
\}

