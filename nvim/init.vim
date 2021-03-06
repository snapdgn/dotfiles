" Plugins will be downloaded under the specified directory.
"
"+++++++++++++++++PLUGINS START+++++++++++++++++++++
"
call plug#begin('~/.config/nvim/vim-plug/')
 Plug 'tpope/vim-surround'
 Plug 'scrooloose/syntastic'
 Plug 'scrooloose/nerdtree'
 Plug 'scrooloose/nerdcommenter'
 Plug 'itchyny/lightline.vim'
 Plug 'morhetz/gruvbox'
 Plug 'machakann/vim-highlightedyank'
 Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
 Plug 'junegunn/fzf.vim'
 Plug 'BurntSushi/ripgrep'
 "markdown preview
 Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
 Plug 'cohama/lexima.vim'
 "Plug 'flazz/vim-colorschemes'
 Plug 'liuchengxu/space-vim-dark'
 "Semantic language support
 Plug 'neovim/nvim-lspconfig'
 Plug 'nvim-lua/lsp_extensions.nvim'
 Plug 'hrsh7th/nvim-cmp'
 Plug 'hrsh7th/cmp-nvim-lsp'
 Plug 'hrsh7th/cmp-buffer'

 Plug 'airblade/vim-gitgutter'
 Plug 'airblade/vim-rooter'
 "Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
 Plug 'rust-lang/rust-clippy'
 Plug 'godlygeek/tabular'
 Plug 'rust-lang/rust.vim'
 Plug 'plasticboy/vim-markdown'
 
 Plug 'vimwiki/vimwiki'
 Plug 'ryanoasis/vim-devicons'
" List ends here. Plugins become visible to Vim after this call.
 call plug#end()

"++++++++++++PLUGINS END+++++++++++++++++++++++++++++++++
"
"

syntax on
set termguicolors     " enable true colors support
"let ayucolor="mirage"
set bg=dark
"let g:gruvbox_transparent_bg = 0
"let g:gruvbox_italic=1
"let g:gruvbox_contrast_dark = 'medium'
"colorscheme PaperColor
colorscheme space-vim-dark
"colorscheme gruvbox

set laststatus=2
"set noshowmode
set nocompatible
filetype plugin on

set autoindent
set autoread
set clipboard=unnamedplus
set complete-=i
set completeopt+=noselect
set confirm
"===GUI==========================================
set number
set relativenumber
set diffopt+=iwhite " No whitespace in vimdiff
"set encoding=UTF-8
" set virtualedit=all

" Show those damn hidden characters
" Verbose: set listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set listchars=nbsp:¬,extends:»,precedes:«,trail:•
set shortmess+=c

"================================================================

"==Cursorline configs======
set cursorline
autocmd InsertEnter * highlight CursorLine guibg=#0f0d05
"autocmd InsertLeave * highlight CursorLine guibg=#ffffff guifg=fg

"set cursorcolumn
"autocmd InsertEnter * highlight CursorColumn ctermfg=White ctermbg=Yellow cterm=bold guifg=white guibg=yellow gui=bold
"autocmd InsertLeave * highlight CursorColumn ctermfg=Black ctermbg=Yellow cterm=bold guifg=Black guibg=yellow gui=NONE

""=======================
" Change style of highlighting (removes that annoying red block of text on
" errors)
hi clear SpellBad
hi SpellBad cterm=underline
hi Normal ctermbg=NONE guibg=NONE
set expandtab
set hidden
set hlsearch
set incsearch
set mouse+=a
set nobackup
set nu
"go to the end of the line in insert mode
set ve+=all
set ve+=onemore
" Give more space for displaying messages
set cmdheight=2

"shows menu and stuff for autocompletion v.imp
set completeopt=menu,menuone,noselect
"=====syntastic error symbol========
let g:syntastic_error_symbol = "✗"
let syntastic_style_error_symbol = "✗"
let g:syntastic_warning_symbol = "∙∙"
let syntastic_style_warning_symbol = "∙∙"

"======================
""======Ale configs support for rust==========
"set omnifunc=ale#completion#OmniFunc
"let g:ale_linters = {'rust': ['analyzer']}
"let g:ale_completion_enabled = 1
"let g:ale_completion_autoimport = 1
"let g:ale_sign_column_always = 1
"let g:ale_fix_on_save = 1
"let g:ale_sign_error = "✗"
"let g:ale_sign_warning = "∙∙"

"let g:ale_fixers = {
    "\ ‘*’: [‘remove_trailing_lines’, ‘trim_whitespace’],
    "\ ‘rust’: [‘rustfmt’],
"\}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
"below line does same thing as above
"inoremap <silent><expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

"--------------------Shreerun's tips----------------------------------------------
"
"
" from http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
endif
if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif
"
"
"automatically jump to the end of text you pasted

vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

"--------------------end of shreerun's tips------------------------------
set ruler
set shiftwidth=4
set smartindent
set smarttab
set so=10
set softtabstop=4
set tabstop=4
set textwidth=80
set title
set updatetime=100
set wrap
set undodir=$HOME/.vim/undo
set undofile

"Wildmenu
set wildmenu
set wildmode=list:longest
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor

"proper search
set incsearch
set ignorecase
set smartcase
set gdefault


" rust
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -selection clipboard'


""==========NERD TREE==============="
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
"let NERDTreeQuitOnOpen = 1


 " This does the same thing i.e. quit nerd if its the last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Jump to last edit position on opening file
  " https://stackoverflow.com/questions/31449496/vim-ignore-specifc-file-in-autocommand
if has("autocmd")
  au BufReadPost * if expand('%:p') !~# '\m/\.git/' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

"==================LSP_EXTENSION CONFIGS=====''
"On cursor hover, get hints for current line:
autocmd CursorHold,CursorHoldI *.rs :lua require'lsp_extensions'.inlay_hints{ only_current_line = true }


lua require'lspconfig'.rust_analyzer.setup{}
luafile ~/.config/nvim/lsp_config.lua

"===========LIGHTLINE CONFIGS================"

let g:lightline = {
      \ 'colorscheme': 'molokai',
      \ }



"=============MAPPINGS===========================


"inoremap <Leader>, <ESC>:w<CR>
let g:C_Ctrl_j = 'off' "turns off the default thing mapping of ctrl+j of vim(move n line downwords)
"inoremap <C-j> <ESC>:w<CR>
"inoremap <C-j> <ESC>:w<CR>
"inoremap <C-k> <ESC>:w<CR>
inoremap <C-j> <ESC>
inoremap <C-k> <ESC>
inoremap <C-l> <ESC>

"inoremap kk <ESC>:w<CR>
let mapleader = ","
set timeout timeoutlen=1500
"map <C-H> <C-W><C-H>
"map <C-J> <C-W><C-J>
"map <C-K> <C-W><C-K>
"map <C-L> <C-W><C-L>
"map <C-m> :cprevious<CR>
"map <C-n> :cnext<CR>
"map <F4> :noh<CR>
"map <F7> gg=G<C-o><C-o>
map <Leader>, :w<CR>
"map <Leader>f <ESC>:FZF<CR>
"map <Leader>g <ESC>:Rg<CR>
map <Leader>[ :bprevious<CR>
map <Leader>] :bnext<CR>
map <Leader>k :NERDTreeToggle <CR>
map <leader>cc :NERDCommenterComment
" <leader>s for Rg search
map <leader>s :Rg<CR>
map <leader>m :MarkdownPreview<CR>

"map <Leader>l :buffers list<CR>
"map <leader>a :cclose <bar> lclose <bar> pclose<CR>
"map <leader>s <C-w>s<CR><C-w><C-J>:term<CR>i
"map <leader>v <C-w>v<CR><C-w><C-L>:term<CR>i
"map <leader>~ :set spell spelllang=en_gb<CR>
"map <silent> <C-j> <Plug>(ale_next_wrap)
"map <silent> <C-k> <Plug>(ale_previous_wrap)
"nnoremap <C-n> :norm
map  <C-l> :tabn<CR>
map  <C-h> :tabp<CR>
map  <C-n> :tabnew<CR>

" No arrow keys --- force yourself to use the home row
nnoremap <up> <nop>
nnoremap <down> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

vnoremap <up> <nop>
vnoremap <down> <nop>
vnoremap <left> <nop>
vnoremap <right> <nop>
" No Backspace /Delete/Pageup/PageDown keys

set backspace=0
set backspace=indent,eol,start

nnoremap <delete> <nop>
inoremap <delete> <nop>

nnoremap <PageUp> <nop>
inoremap <PageUp> <nop>
vnoremap <PageUp> <nop>

nnoremap <PageDown> <nop>
inoremap <PageDown> <nop>
vnoremap <PageDown> <nop>

" Left and right can switch buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>

" Move by line
"nnoremap j gj
"nnoremap k gk

" Quickly move current line above or below
nnoremap [e  :<c-u>execute 'move -1-'. v:count1<cr>
nnoremap ]e  :<c-u>execute 'move +'. v:count1<cr>

"Quickly add empty lines
nnoremap [<space>  :<c-u>put! =repeat(nr2char(10), v:count1)<cr>'[
nnoremap ]<space>  :<c-u>put =repeat(nr2char(10), v:count1)<cr>
"Now 5[<space> inserts 5 blank lines above the current line.

" Make double-<Esc> clear search highlights
nnoremap <silent> <Esc><Esc> <Esc>:nohlsearch<CR><Esc>

" Open hotkeys
nnoremap <C-p> :Files<CR>
nnoremap <C-o> :Buffers<CR>
nnoremap <C-g> :GFiles<CR>
nnoremap <C-f> :Rg<CR> 

"Delimiters
inoremap {} <CR>{<CR>}<Up><CR>

" Search results centered please
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz



