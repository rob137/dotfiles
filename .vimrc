set nocompatible    
filetype plugin indent on " https://vi.stackexchange.com/a/10125/25047
syntax on
au BufReadPost *.tsx set syntax=javascript " To play nice with TypeScript
au BufReadPost *.ts set syntax=javascript
set hidden
set foldmethod=indent
" set foldlevelstart=20 " Ensure all folds are unfolded when a file is opened
set cursorline " Highlight current line of cursor
set scrolloff=4 " Line margin from top/bottom
set path+=** " For file search - display all matching files when we tab complete
set wildmenu " Provides tab-completion for all file-related tasks
set wildignore=*/.git/*,*/node_modules/*,*/dist/*,*/build/*,*/coverage/*
set hlsearch incsearch " Highlight search results, and jump to them as you type
set ignorecase smartcase " Ignore case of search query unless a capital letter is used
set lazyredraw " Don't redraw while executing macros (good for performance)
set showmatch " Show matching brackets when text indicator is over them
set number relativenumber
set colorcolumn=80 " Vertical column to show 80 chars
set autoindent " Figure out the correct indentation when creating a new line
set shiftwidth=2 " Number of spaces used for indentation
" set expandtab " Convert tab to spaces - disabled as work prefer tabs
set tabstop=2 " Display tabs as 2 spaces wide
set list " This and next two highlight tabs with special character
set listchars=tab:··
highlight SpecialKey ctermfg=1
let g:netrw_liststyle = 3 " Make file explorer default to tree view 
let g:netrw_preview   = 1 " Ensure file explorer preview pane is vertical and to right of screen
let g:netrw_alto      = 0 " (netrw_alto=0 is required to keep it on right while splitright is set)
let g:netrw_winsize   = 15 " Make file explorer smaller by default
colorscheme seattle
set splitright splitbelow " Open vertical splits to right and horizontal splits below
set history=1000 undolevels=1000
" TODO figure out for linux (ideally cross-platfor solution using if/then)
set clipboard=unnamed " Content of yank goes to system clipboard (mac only, sadly)
autocmd FileType gitcommit set textwidth=72 " Break to new line at 72 characters
autocmd FileType gitcommit set colorcolumn+=51 " Also colour the 51st column (for titles)
" Next is replaced by airline
" set statusline+=%F " Always show full file path in status bar at bottom
let g:airline_theme="badwolf"
" Edit coc config settings using :CocConfig - coc-pretter and coc-eslint
" Github pages are very useful
let g:coc_global_extensions = [
  \ 'coc-html',
  \ 'coc-css',
  \ 'coc-docker',
  \ 'coc-tsserver',
  \ 'coc-json',
  \ 'coc-xml',
  \ 'coc-yaml',
  \ 'coc-sh',
  \ 'coc-eslint',
  \ 'coc-prettier',
  \ ]
call plug#begin('~/.vim/plugged')
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'cakebaker/scss-syntax.vim'
" Plug 'dense-analysis/ale' " TODO: consider whether CoC could replace ALE (bloat)
" Plug 'maxboisvert/vim-simple-complete' " lightweight as-you-type keyword completion and tab complete
Plug 'neoclide/coc.nvim', {'branch': 'release'} " Bloaty, makes me sad
" Plug 'ervandew/supertab' " for as-you-type completion suggestions
Plug 'vim-airline/vim-airline' " Fancy status bar at bottom of screen
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive' " g? for keybindings in git splits/windows
Plug 'airblade/vim-gitgutter' " marks untracked changes in left column - 'GitGutter' will refresh the gutter
Plug 'rbong/vim-flog' " Flog/Flogsplit to view branch history
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-surround' " Use with cs'{ to change surrounding '' to {}
Plug 'JamshedVesuna/vim-markdown-preview'
" Plug 'rafi/awesome-vim-colorschemes'
" Plug 'flazz/vim-colorschemes'
call plug#end()
source ~/.vimrc.coc " CoC settings from https://github.com/neoclide/coc.nvim - note includes many keybinding
com! RefreshVim source ~/.vimrc | PlugClean | PlugInstall 
" Other autofixers
nmap =j :%!python -m json.tool<CR>
com! Formatjson %!python -m json.tool " Format JSON (use :FormatJson)
au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null " Format xml (use gg=G)
" Markdown preview - note requires grip to work https://github.com/joeyespo/grip
" Use ctrl+p in markdown files to open in browser
let vim_markdown_preview_github=1
let vim_markdown_preview_browser='Google Chrome'
:set directory=$HOME/.vim/swapfiles// " Swap file storage.  Must first run 'mkdir ~/.vim/swapfiles'
if v:version >= 700 " Prevent window changing position when switching buffers
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif
" Disable arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>
" Disable ex mode
nnoremap Q <Nop> 
" for escaping insert mode
imap jj <Esc>






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

" Ale autcompletion was too slow to be helpful (possibly Docker Desktop
" hogging resources?)
" - Wouldn't work without running ':source ~/.vimrc on opening vim
" - Way too slow
"let g:ale_linters = {
"			\	'typeScript': ['tsserver'],
"			\	'javascript': ['tsserver'],
"\}
" set omnifunc=syntaxcomplete#Complete " Allows language-specific autocompletion in insert mode with CTRL-x CTRL-o
"set omnifunc=ale#completion#OmniFunc
"let g:ale_completion_enabled = 1 " can't seem to get this to work :(
"let g:ale_completion_delay = 10 
"let g:ale_completion_tsserver_autoimport = 1
" let g:ale_fix_on_save = 1 " Note - currently only works if vimrc is sourced on opening
" " I've previously just used '*': ['prettier']
" let g:ale_fixers = {
" \   'css': ['prettier'],
" \   'less': ['prettier'],
" \   'json': ['eslint'],
" \   'javascript': ['eslint'],
" \}
" nmap <silent> <leader>]a :ALENext<cr>
" nmap <silent> <leader>[a :ALEPrevious<cr>







" Having fun with colorschemes
" Randomise colorschemes
function RandomColorScheme()
  let mycolors = split(globpath(&rtp,"**/colors/*.vim"),"\n") 
  exe 'so ' . mycolors[localtime() % len(mycolors)]
  unlet mycolors
endfunction
" Assign random colorscheme for vim on opening
" call RandomColorScheme()
" Shortcuts / keybinds
:command NewColor call RandomColorScheme()
:command NC call RandomColorScheme()
nmap <silent> <leader>n :NC<cr>

" ':ANC' to add a pretty color to a version-controlled list for later
" shortlisting (probably make a repository of the best ones)
function AddToNiceColors()
	redir >>~/dotfiles/nice-vim-colors.txt|silent colorscheme|redir END
endfunction
" Shortcuts / keybinds
:command ANC call AddToNiceColors()
nmap <silent> <leader>N :ANC<cr>






" use tab for autocompletion rather than ctrl+p
function! InsertTabWrapper()
	let col = col('.') - 1
	if !col || getline('.')[col - 1] !~ '\k'
		return "\<tab>"
	else
		return "\<c-p>"
	endif
endfunction
inoremap <expr> <s-tab> InsertTabWrapper()
inoremap <tab> <c-n>
