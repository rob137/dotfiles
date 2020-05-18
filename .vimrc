" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Line numbers
set number relativenumber 

" Indentation - currently 2 spaces when tab is pressed
set smartindent shiftwidth=2 expandtab

" Vertical column to show 80 chars
set colorcolumn=80
highlight ColorColumn guibg=LightRed

" Highlight and jump to search results as you type
set hlsearch incsearch

" Ignore case of search query unless a capital letter is used
set ignorecase smartcase

" Enable syntax highlighting
syntax on

" https://vi.stackexchange.com/a/10125/25047
filetype plugin indent on 

" Make file explorer smaller by default
let g:netrw_preview   = 1
let g:netrw_winsize   = 15

" Color scheme using https://github.com/crusoexia/vim-monokai
" ~/.vim/colors/monokai.vim
colorscheme monokai
set background=dark

" For folding functions etc
set foldmethod=syntax
" https://vim.fandom.com/wiki/All_folds_open_when_opening_a_file
set foldlevelstart=20

" Longer q: and undo histories
set history=1000
set undolevels=1000

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
" Not using this in current role.
" let g:ale_fix_on_save = 1
" let g:ale_fixers = {
" \   '*': ['prettier'],
" \}

" Open URL under cursor in Chrome when 'gx' is typed
" Note that for \"google-chrome\" on MacOS I use the alias 'open -a \"Google Chrome\"'
" Currently broken:
" https://github.com/vim/vim/issues/4738#issuecomment-612354457
" let g:netrw_browsex_viewer="google-chrome"
