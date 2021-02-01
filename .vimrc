set nocompatible
filetype plugin indent on " https://vi.stackexchange.com/a/10125/25047
syntax on
map <Space> <Leader>
au BufReadPost *.tsx set syntax=javascript " To play nice with TypeScript
au BufReadPost *.ts set syntax=javascript
set hidden " Unsaved buffers are ok
set backspace=indent,eol,start " Normal backspace (in case I'm ever on an old system)
set foldmethod=indent
" set foldlevelstart=20 " Ensure all folds are unfolded when a file is opened
set cursorline " Highlight current line of cursor
set ruler " show postion at bottom of screen
set showcmd " show what I'm typing at bottom of screen
set scrolloff=4 " Line margin from top/bottom
set path+=** " For file search - display all matching files when we tab complete
set wildmenu " Provides tab-completion for all file-related tasks
set wildignore=*/.git/*,*/node_modules/*,*/dist/*,*/build/*,*/coverage/*
set hlsearch incsearch " Highlight search results, and jump to them as you type
set ignorecase smartcase " Ignore case of search query unless a capital letter is used
set lazyredraw " Don't redraw while executing macros (good for performance)
set ttyfast " needed?
set showmatch " Show matching brackets when text indicator is over them
set number relativenumber
set colorcolumn=80 " Vertical column to show 80 chars
set autoindent " Figure out the correct indentation when creating a new line
set shiftwidth=2 " Number of spaces used for indentation
set shiftround " round to multiple of shiftwidth
" set expandtab " Convert tab to spaces - disabled as work prefer tabs
set tabstop=2 " Display tabs as 2 spaces wide
set list " This and next two highlight tabs with special character
set listchars=tab:··
highlight SpecialKey ctermfg=1
let g:netrw_liststyle = 3 " Make file explorer default to tree view
let g:netrw_preview   = 1 " Ensure file explorer preview pane is vertical and to right of screen
let g:netrw_alto      = 0 " (netrw_alto=0 is required to keep it on right while splitright is set)
let g:netrw_winsize   = 15 " Make file explorer smaller by default
let g:netrw_banner    = 0
command! ShowFile let @/=expand("%:t") | execute 'Lexplore' expand("%:h") | normal n " show file in netrw
set splitright splitbelow " Open vertical splits to right and horizontal splits below
set history=1000 undolevels=1000
set undofile
" TODO figure out for linux (ideally cross-platfor solution using if/then)
set clipboard=unnamed " Content of yank goes to system clipboard (mac only, sadly)
autocmd FileType gitcommit set textwidth=72 " Break to new line at 72 characters
autocmd FileType gitcommit set colorcolumn+=51 " Also colour the 51st column (for titles)
set laststatus=2
" Edit coc config settings using :CocConfig
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
			\ 'coc-stylelintplus'
			\ ]
call plug#begin('~/.vim/plugged')
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-fugitive' " g? for keybindings in git splits/windows
Plug 'airblade/vim-gitgutter'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-surround' " Use with cs'{ to change surrounding '' to {}
Plug 'JamshedVesuna/vim-markdown-preview'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'KurtPreston/vimcolors'
call plug#end()
set t_Co=256 " 256 colors (not sure it makes any difference)
colorscheme gruvbox
set bg=dark
source ~/.vimrc.coc " CoC settings from https://github.com/neoclide/coc.nvim - note includes many keybinding
com! RefreshVim source ~/.vimrc | PlugClean | PlugInstall
" Other autofixers
nmap <leader>j :%!python -m json.tool<CR>
com! Formatjson %!python -m json.tool " Format JSON (use :FormatJson)
au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null " Format xml (use gg=G)
" Markdown preview - note requires grip to work https://github.com/joeyespo/grip
" Use ctrl+p in markdown files to open in browser
let vim_markdown_preview_github=1
let vim_markdown_preview_browser='Google Chrome'
set noswapfile
if v:version >= 700 " Prevent window changing position when switching buffers
	au BufLeave * let b:winview = winsaveview()
	au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif
" Disable ex mode
nnoremap Q <Nop>
" for escaping insert mode
imap jk <Esc>
imap kj <Esc>
" Editing vimrc
map <Leader>ev :tabedit $MYVIMRC<CR>
map <Leader>es :source $MYVIMRC<CR>
" console.log/error/clear
imap <Leader>cl console.log();<Esc>==f(a
vmap <Leader>cl yo<Leader>cl<Esc>p
nmap <Leader>cl yiwo<Leader>cl<Esc>==f(p
imap <Leader>ce console.error();<Esc>==f(a
vmap <Leader>ce yo<Leader>ce<Esc>p
nmap <Leader>ce yiwo<Leader>ce<Esc>==f(p
imap <Leader>cc console.clear();<Esc>
vmap <Leader>cc o<Leader>cc
nmap <Leader>cc o<Leader>cc
imap <Leader>co console.count();<Esc>
vmap <Leader>co o<Leader>co
nmap <Leader>co o<Leader>co
" Open buffer / Ack / find in new tab / split
noremap <leader>tb :tabedit<space>\|<space>b<space>
noremap <leader>vb :vsplit<space>\|<space>b<space>
noremap <leader>ta :tabedit<space>\|<space>Ack<space>-i<space>
noremap <leader>va :vsplit<space>\|<space>Ack<space>-i<space>
noremap <leader>tf :tabedit<space>\|<space>find<space>
noremap <leader>vf :vsplit<space>\|<space>find<space>
noremap <leader>a :Ag<CR>
" View diff
noremap <leader>gd :Gdiffsplit<space>
" Search current file for whole word
nnoremap <leader>/ /\<\><left><left>
" Vertical resize
noremap <leader>v5 :vertical<space>resize<space>50<CR>
noremap <leader>v4 :vertical<space>resize<space>40<CR>
noremap <leader>v3 :vertical<space>resize<space>30<CR>

nnoremap <F5> :UndotreeToggle<cr>

nnoremap <space>html :-1read<space>~/.vim/snippets/html.txt<CR>
nnoremap <space>classcomponent :-1read<space>~/.vim/snippets/classcomponent.txt<CR>
nnoremap <space>functionalcomponent :-1read <space>~/.vim/snippets/functionalcomponent.txt<CR>

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

" Ale autocompletion was too slow to be helpful (possibly Docker Desktop
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


" For 3 way git merges
" Remove braces / equals signs
let @m = '/<<<<<ztddk/=====ddk/>>>>dd'
" Add spaces between .po file declarations where missing
let @p = '/msgidkyl/\"\n\#:j0O'




" use tab for autocompletion rather than ctrl+p
" function! InsertTabWrapper()
" 	let col = col('.') - 1
" 	if !col || getline('.')[col - 1] !~ '\k'
" 		return "\<tab>"
" 	else
" 		return "\<c-p>"
" 	endif
" endfunction
" inoremap <expr> <s-tab> InsertTabWrapper()
" inoremap <tab> <c-n>

" Xml formatter - brittle!  Switch on, use, switch off.
" com! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
" nnoremap = :FormatXML<Cr>
