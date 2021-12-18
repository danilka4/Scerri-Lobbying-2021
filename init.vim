call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate' " Completes parenthesis/brackets/quotes/etc.
Plug 'karoliskoncevicius/vim-sendtowindow'
Plug 'neovim/nvim-lspconfig' " LSP installation

" Auto-completion engine
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
Plug 'ms-jpq/coq.thirdparty', {'branch': '3p'}

call plug#end()

" autocomplete settings
let g:coq_settings = { 'display.icons.mode': 'none' }
let g:coq_settings = { 'auto_start': 'shut-up'}


" r lsp stuff
lua require'lspconfig'.r_language_server.setup{}

"
" general stuff
"

" sets up numbering
set nu rnu

" coloring on
syntax on
colorscheme delek

" sets tab size
set tabstop=4
set softtabstop=4
set expandtab

" show command
set showcmd

" sets the lines away from top to bottom during which scrolling begins
set so=5

" creates wildmenu
set wildmenu

" starts up vim at last known position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" redraw tweak for faster micros
set lazyredraw

" removes whitespace upon saving a file ~~and then returns to last seen location~~
autocmd BufWritePre * :%s/\s\+$//e

" sets :Q to quit
command! Q q

" sets path to be everything
set path+=**
" sets leader to comma
let mapleader = " "

" allows for the opening and closing of init.vim from other files
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
" autocmd! bufwritepost init.vim source %

" nohlsearch remap
nnoremap <leader><space> :nohlsearch<CR>

" makes semicolon into a colon
nnoremap ; :

" sets up better searching
set incsearch   " highlight all as you type
set ignorecase  " ignore case
set smartcase   " don't ignore uppercase

" uses arrowkeys to switch screens
nnoremap <C-h> <C-W>h
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-l> <C-W>l

set splitright splitbelow " sets splitting priorities


"
" all presets for R/R-adjacent
"

let g:sendtowindow_use_defaults=0 " prevents window setting from using defaults


autocmd FileType r,rmd call SetROptions()
function SetROptions()
        inoremap <buffer><silent> ` ```{r}<CR>```<Esc>O
        inoremap <buffer><silent> `` `
        set iskeyword+=.
        map <Leader>r :new term://bash<CR>iR<CR><C-\><C-n>G<C-w>k
        map <Leader>q :wincmd j<CR>:q<CR>
        autocmd VimEnter * split
        autocmd VimEnter * term
        autocmd VimEnter * norm G
        autocmd VimEnter * wincmd k
        au VimEnter * call chansend(4, "R\n")
        inoremap <buffer><silent> ;s <SPACE>%>% " Creates a pipe
        inoremap <buffer><silent> ;a <SPACE><-<SPACE> " Creates an arrow
        nmap <Leader>kk :call chansend(4, "rmarkdown::render('" . expand("%:p") . "')\n")<CR> " knits to default
        nmap K bve:call chansend(4, "?" . a:visualmode() . "\n")<CR>
        nmap <Leader>l ^Vf(%<bar><Plug>SendDownV " Sends line down
        vmap <Leader>s SendDownV
        nmap <Leader>c /```<CR>?```{r}<CR>jV/```<CR>k<bar><Plug>SendDownV<bar>:nohlsearch<CR> " Sends chunk
        nmap <Leader>j /```{r}<CR>j:nohlsearch<CR> " Go down a chunk
        nmap <Leader>k ?```{r}<CR>nj:nohlsearch<CR> " Go up a chunk
        nmap <Leader>n ?```{r}<CR>jV/```<CR>k<bar><Plug>SendDownV<bar>/```{r}<CR>j:nohlsearch<CR> " Do current chunk and go down a chunk
        nmap <Leader>u k/```$<CR>:nohlsearch<CR>:call FirstToTarget(line("."))<CR>
        nmap <Leader>a :call FirstToTarget(line("$"))<CR>
        vmap <Leader>v <Plug>SendDownV
endfunction

function FirstToTarget(cur)
        let lin = 1
        let inchunk = 0
        while lin <= a:cur
                if (inchunk == 1) && (getline(lin) != "```")
                        call chansend(4, getline(lin) . "\n")
                elseif inchunk == 1
                        let inchunk = 0
                elseif (inchunk == 0) && (getline(lin) == "```{r}")
                        let inchunk = 1
                endif
                let lin += 1
        endwhile
endfunction

"
" presets for tex stuff
"
au FileType plaintex noremap  <silent> j gj
au FileType plaintex noremap  <silent> k gk
