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
set cuc " Vertical highlight on cursor
set guicursor= " non-blinking cursor
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
set timeoutlen=600 " Reduce key mapping delays; default 1000ms
set showmatch " Show matching brackets when text indicator is over them
set number relativenumber
set colorcolumn=80 " Vertical column to show 80 chars
set autoindent " Figure out the correct indentation when creating a new line
set shiftwidth=2 " Number of spaces used for indentation
set shiftround " round to multiple of shiftwidth
set expandtab " Convert tab to spaces
set tabstop=2 " Display tabs as 2 spaces wide
set list " This and next two highlight tabs with special character
set listchars=tab:··
set visualbell
set noerrorbells
highlight SpecialKey ctermfg=1
let g:netrw_liststyle = 3 " Make file explorer default to tree view
let g:netrw_preview   = 1 " Ensure file explorer preview pane is vertical and to right of screen
let g:netrw_alto      = 0 " (netrw_alto=0 is required to keep it on right while splitright is set)
let g:netrw_winsize   = 15 " Make file explorer smaller by default
let g:netrw_banner    = 0
command! ShowFile let @/=expand("%:t") | execute 'Lexplore' expand("%:h") | normal n " show file in netrw
nnoremap <leader>sh :ShowFile<CR> " for 'show'
set splitright splitbelow " Open vertical splits to right and horizontal splits below
set history=1000 undolevels=1000
set undofile
 set undodir=~/.vim/undodir
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
Plug 'editorconfig/editorconfig-vim'
Plug 'arcticicestudio/nord-vim'
Plug 'altercation/vim-colors-solarized'
Plug 'morhetz/gruvbox'
Plug 'vim-test/vim-test'
Plug 'szw/vim-maximizer'
Plug 'junegunn/goyo.vim'
Plug 'ap/vim-css-color'
Plug 'NikolayFrantsev/jshint2.vim'
Plug 'tpope/vim-commentary'
Plug 'hashivim/vim-terraform'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
call plug#end()
set t_Co=256 " 256 colors (not sure it makes any difference)
" colorscheme morning
colorscheme morning
set bg=dark
source ~/.vimrc.coc " CoC settings from https://github.com/neoclide/coc.nvim - note includes many keybinding
" for CoC / Go 
" disable all linters as that is taken care of by coc.nvim
let g:go_diagnostics_enabled = 0
let g:go_metalinter_enabled = []
" source ~/.vimrc.local
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

" Recognise txt files,  set linebreak on txt and markdown files
autocmd BufNewFile,BufRead    *.txt     setf text
autocmd FileType text setlocal linebreak foldlevel=999
autocmd FileType markdown setlocal linebreak foldlevel=999

if v:version >= 700 " Prevent window changing position when switching buffers
	au BufLeave * let b:winview = winsaveview()
	au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif
" Disable ex mode
nnoremap Q <Nop>
" for escaping insert mode
imap jk <Esc>
imap kj <Esc>
" Editing / sourcing rcs
map <Leader>ev :tabedit ~/.vimrc<CR>
map <Leader>es :source ~/.vimrc<CR>
map <Leader>ez :tabedit ~/.zshrc<CR>
map <Leader>zsh :tabedit ~/.zshrc<CR>

" Todos
map <leader>td :tabedit ~/notes/todo.txt<CR>
map <leader>tdl :tabedit ~/notes/todo_life.txt<CR>
map <leader>tdb :split ~/notes/todo.txt<CR><C-W>J<C-W>100-ggj<C-W><C-W>
map <leader>br :tabedit ~/notes/branches.txt<CR>
map <leader>acc :tabedit ~/notes/accounts.txt<CR>

map <leader>glo :tabedit ~/notes/glossary.txt<CR>
map <leader>re :tabedit ~/notes/refresher.txt<CR>

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
imap <Leader>kcl console.log('\u001b[2J\u001b[0;0H');<Esc>
vmap <Leader>kcl ggO<Leader>kcl
nmap <Leader>kcl ggO<Leader>kcl
  

" General
nnoremap <leader>qq :q<CR>
nnoremap <leader>QQ :q!<CR>
nnoremap <leader>qa :qa<CR>
nnoremap <leader>QA :qa!<CR>
nnoremap <leader>wq :wq<CR>
nnoremap <leader>WQ :wq!<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>W :w!<CR>
nnoremap <leader>o :only<CR>
nnoremap <leader>tt :tabe<space>
nnoremap <leader>vs :vs<space>
nnoremap <leader>sp :sp<space>
nnoremap <leader>ee :edit<space>
nnoremap <leader>EE :edit!<CR>
nnoremap <leader>ls :ls<cr>
nnoremap <leader>bb :buffer<space>
nnoremap <leader>bu :Buffers<CR>
nnoremap <leader>bd :bd<space>
nnoremap <leader>BD :bd!<space>
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>co :Commits<CR>
nnoremap <leader>ff :find<space>
nnoremap <leader>fi :Files<CR>
nnoremap <leader>hi :History<CR>
nnoremap <leader>he :Help<CR>
nnoremap <leader>ma :Marks<CR>
nnoremap <leader>aa :Ack<space>
nnoremap <leader>ag :Ag<CR>
nnoremap <leader>to :tabonly<CR>
nnoremap <leader>tc :tabclose<CR>
nnoremap <leader>nh :nohlsearch<cr>
nnoremap <leader>nr :set norelativenumber<cr>
nnoremap <leader>rn :set relativenumber<cr>
nnoremap <leader>ts :tab<space>split<cr>
nnoremap <leader>zen :Goyo<cr>
nnoremap <leader>r :e<CR>
nnoremap <leader>R :w<CR>:e<CR>
nnoremap <leader>ch :tabedit<space>ch><CR>ggdG:read<space>!ch<space>
nnoremap <leader>ch :tabedit<space>ch><CR>ggdG:read<space>!ch<space>
nnoremap <leader>zz zR
nnoremap <leader>bde :bufdo<space>e<CR>
nnoremap <leader>pwd :pwd<CR>
" Copy absolute path of current buffer to clipboard
nnoremap <Leader>cp :let @+=expand('%:p')<CR>
nnoremap <leader>cd :cd<space>
nnoremap <leader>lb :set<space>linebreak<CR>
nnoremap <leader>nlb :set<space>nolinebreak<CR>
nnoremap <leader>wr :set<space>wrap<CR>
nnoremap <leader>nwr :set<space>nowrap<CR>
nnoremap <leader>mks :mks<CR>
nnoremap <leader>MKS :mks!<CR>
nnoremap <leader>lh :GitGutterLineHighlightsEnable<CR>
nnoremap <leader>nlh :GitGutterLineHighlightsDisable<CR>
nnoremap <leader>pi :PlugInstall<CR>
nnoremap <leader>pc :PlugClean<CR>



" Sort multiline lists within brackets
nnoremap <leader>so[ vi[:sort<CR>
nnoremap <leader>so{ vi{:sort<CR>
nnoremap <leader>so( vi(:sort<CR>

" CoC
nnoremap <leader>ft :CocCommand tslint.fixAllProblems<CR>

" prettier format file
nnoremap <leader>fp :CocCommand prettier.formatFile<CR>
" prettier format selection - not working at present, will reformat whole file
" https://github.com/neoclide/coc-prettier/issues/120
vmap <leader>fp  <Plug>(coc-format-selected)

" JSHint
nnoremap <leader>js :'<,'>JSHint

" Debugger
let g:vimspector_enable_mappings = 'HUMAN'
fun GotoWindow(id)
  call win_gotoid(a:id)
endfun
noremap <leader>mt :MaximizerToggle!<CR>
noremap <leader>db :call vimspector#Launch()<CR>
noremap <leader>dc :call GotoWindow(g:vimspector_session_windows.code)<CR>
noremap <leader>dt :call GotoWindow(g:vimspector_session_windows.tagpage)<CR>
noremap <leader>dv :call GotoWindow(g:vimspector_session_windows.variables)<CR>
noremap <leader>dw :call GotoWindow(g:vimspector_session_windows.watches)<CR>
noremap <leader>ds :call GotoWindow(g:vimspector_session_windows.stack_trace)<CR>
noremap <leader>do :call GotoWindow(g:vimspector_session_windows.output)<CR>      
noremap <leader>dr :call vimspector#Reset()<CR> 
noremap <leader>ws /\s\+\n




" Open buffer / Ack / find in new tab / split
nnoremap <leader>tb :tabedit<space>\|<space>b<space>
nnoremap <leader>vb :vsplit<space>\|<space>b<space>
nnoremap <leader>sb :split<space>\|<space>b<space>
nnoremap <leader>ta :tabedit<space>\|<space>Ack<space>-i<space>
nnoremap <leader>va :vsplit<space>\|<space>Ack<space>-i<space>
nnoremap <leader>sa :split<space>\|<space>Ack<space>-i<space>
nnoremap <leader>tf :tabedit<space>\|<space>find<space>
nnoremap <leader>vf :vsplit<space>\|<space>find<space>
nnoremap <leader>sf :split<space>\|<space>find<space>
" View diff
nnoremap <leader>gds :Gdiffsplit<space>
" Search current file for whole word
nnoremap <leader>/ /\<\><left><left>
" Vertical resize
noremap <leader>v5 :vertical<space>resize<space>50<CR>
noremap <leader>v4 :vertical<space>resize<space>40<CR>
noremap <leader>v3 :vertical<space>resize<space>30<CR>
" Fugitive
noremap <leader>g :Git<CR>
noremap <leader>gbm :Git<space>branch<space>-m<space>
noremap <leader>gbd :Git<space>branch<space>-D<space>
noremap <leader>gap :Git<space>add<space>--patch<space>
noremap <leader>gap. :Git<space>add<space>--patch<space>.<CR>
noremap <leader>ga. :Git<space>add<space>.<CR>
noremap <leader>ga% :Git<space>add<space>%<CR>
noremap <leader>gc :Git<space>commit<CR>
noremap <leader>gcnv :Git<space>commit<space>--no-verify<CR>
noremap <leader>gca :Git<space>commit<space>--amend<CR>
noremap <leader>gcane :Git<space>commit<space>--amend<space>--no-edit<CR>
noremap <leader>gcanv :Git<space>commit<space>--amend<space>--no-verify<CR>
noremap <leader>gcanenv :Git<space>commit<space>--amend<space>--no-edit<space>--no-verify<CR>
noremap <leader>gs :Git<CR>
noremap <leader>gsh :Git<space>stash<CR>
noremap <leader>gshp :Git<space>stash<space>pop<CR>
noremap <leader>gpoh :Git<space>push<space>origin<space>HEAD<CR>
noremap <leader>gpohf :Git<space>push<space>origin<space>HEAD<space>--force<CR>
noremap <leader>gch :Git<space>checkout<space>
noremap <leader>gch- :Git<space>checkout<space>-<CR>
noremap <leader>gchd :Git<space>checkout<space>develop<CR>
noremap <leader>gchm :Git<space>checkout<space>master<CR>
noremap <leader>gchh :Git<space>checkout<space>HEAD^<CR>
noremap <leader>gchb :Git<space>checkout<space>-b<CR>
noremap <leader>gchf :Git<space>chec<space>uuu/eas-
noremap <leader>gcd :Git<space>chec<space>uuu<CR>
noremap <leader>gcm :Git<space>chec<space>uuu<CR>
noremap <leader>gpl :Git<space>pull<CR>
noremap <leader>gf :Git<space>fetch<CR>
noremap <leader>gps :Git<space>push<space>
noremap <leader>grb :Git<space>rebase<space>
noremap <leader>grb- :Git<space>re<space>uuuCR>
noremap <leader>grbd :Git<space>re<space>uuu<CR>
noremap <leader>grbm :Git<space>re<space>uuu<CR>
noremap <leader>grbi :Git<space>re<space>uuuinteractive<space>
noremap <leader>grmc :Git<space>remove<space>--cached<CR>
noremap <leader>grs :Git<space>reset<space>
noremap <leader>grsh :Git<space>reset<space>HEAD^<CR>
noremap <leader>gd :Git<space>diff<space>
noremap <leader>gd. :Git<space>diff<space>.<CR>
noremap <leader>gd% :Git<space>diff<space>%<CR>
noremap <leader>gdh :Git<space>diff<space>HEAD^<CR>
noremap <leader>gdd :Git<space>diff<space>develop<CR>
noremap <leader>gdm :Git<space>diff<space>master<CR>
noremap <leader>gddno :Git<space>diff<space>develop<space>--relative<space>--name-only<CR>
noremap <leader>gdmno :Git<space>diff<space>master<space>--relative<space>--name-onl<CR>
noremap <leader>gdhno :Git<space>diff<space>HEAD^<space>--relative<space>--name-only<CR>
noremap <leader>gdno :Git<space>diff<space>--relative<space>--name-only<CR>
noremap <leader>gl :Git<space>log<CR>
noremap <leader>glog :Glog<space>--<CR>
noremap <leader>glog% :Glog<space>--<space>%<CR>
noremap <leader>grf :Git<space>reflog<CR>
noremap <leader>gsh :Git<space>stash<CR>
noremap <leader>gshp :Git<space>stash<space>pop<CR>
" vim only
noremap <leader>gbl :Git blame<CR>
noremap <leader>gblp :Git blame<CR><C-w><C-w>
noremap <leader>gcl :Gclog<CR>
noremap <leader>sfjs :set filetype=json<CR>
noremap <leader>sfja :set filetype=javascript<CR>

"Quickfix window
noremap [q :cprev<CR>
noremap ]q :cnext<CR>
noremap [a :prev<CR>
noremap ]a :next<CR>
noremap [b :bprev<CR>
noremap ]b :bnext<CR>
noremap [q :cb<CR>
noremap ]q :cn<CR>


nnoremap <F5> :UndotreeToggle<cr>

" Make tmux display current filename in statusline - doesn't update when
" switching buffers / tabs /windows
" autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%:t"))


" snippets
iab _commit <esc>ICAFF-:<space><esc>hs
iab _html <esc>:-1read<space>~/.vim/snippets/html.txt<CR>
iab _classcomponent <esc>:-1read<space>~/.vim/snippets/classcomponent.txt<CR>
iab _functionalcomponent <esc>:-1read <space>~/.vim/snippets/functionalcomponent.txt<CR>
iab _componentangular <esc>:-1read <space>~/.vim/snippets/componentangular.txt<CR>
iab _arrow const<space>=<space>()<space>=><esc>?t<CR>a
iab _media @media<space>(min-width:px)<space>{<CR>}<esc>?:<CR>a
iab _it <esc>:-1read<space>~/.vim/snippets/it.txt<CR>2f'i
iab _describe <esc>:-1read<space>~/.vim/snippets/describe.txt<CR>2f'i

" Xml formatter - brittle!  Switch on, use, switch off.
" com! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
" nnoremap = :FormatXML<Cr>
