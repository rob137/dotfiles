au BufReadPost *.tsx set syntax=javascript
au BufReadPost *.ts set syntax=javascript
let g:netrw_preview   = 1
let g:netrw_winsize   = 15
syntax on
set background=dark
colorscheme monokai

call plug#begin('~/.vim/plugged')
Plug 'dense-analysis/ale',
Plug 'leafgarland/typescript-vim',
Plug 'peitalin/vim-jsx-typescript'
Plug 'cakebaker/scss-syntax.vim'
call plug#end()

let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['prettier'],
\}

set history=1000
