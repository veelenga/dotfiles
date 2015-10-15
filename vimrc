""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HOTKEYS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","

" edit vimrc quickly
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" copy-paste bindings
vnoremap <C-c> "+yi
vnoremap <C-x> "+cse16
inoremap <C-v> <C-r><C-o>+
set clipboard=unnamedplus

" save with ctrl+s
noremap  <C-s> :w<CR>
inoremap <C-s> <C-o>:w<CR>

let g:quickfix_is_open = 0
function! QuickfixToggle()
    if g:quickfix_is_open
        cclose
        let g:quickfix_is_open = 0
    else
        copen
        let g:quickfix_is_open = 1
    endif
endfunction

" Restores cursor position between editing sessions.
" http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" functional keys
map <F1>  :RainbowParenthesesToggle<CR>
map <F2>  :setlocal spell! spelllang=en_us<CR>
map <F3>  :NERDTreeToggle<CR>
map <F4>  :call QuickfixToggle()<CR>
map <F5>  :make<CR>:copen<CR><CR><C-W><C-P>
map <F6>  :make<CR>:copen<CR><CR>
map <F9>  :make<CR>
map <F10> :TagbarToggle<CR>
map <F12> :!ctags -R ./ <CR>

imap <F1>  <C-o><F1>
imap <F2>  <C-o><F2>
imap <F3>  <C-o><F3>
imap <F4>  <C-o><F4>
imap <F5>  <C-o><F5>
imap <F6>  <C-o><F6>
imap <F7>  <C-o><F6>
imap <F8>  <C-o><F6>
imap <F9>  <C-o><F9>
imap <F10> <C-o><F10>
imap <F11> <C-o><F11>
imap <F12> <C-o><F12>

" resizing windows
if bufwinnr(1)
  map + <C-W><
  map - <C-W>>
  "map < <C-W><
  "map > <C-W>>
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" APPEARANCE AND BEHAVIOR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim, not vi
set nocompatible

" show partial command
set showcmd

" syntax and filetypes
syntax   on
filetype on
filetype indent on
filetype plugin on

set nowrap

set number

" save automatically
set autowriteall

" reload file automatically
set autoread

" make programs
autocmd BufNewFile,BufRead *.rb set makeprg=ruby\ %
autocmd BufNewFile,BufRead *.py set makeprg=python\ %
autocmd BufNewFile,BufRead *.sml set makeprg=sml\ %
autocmd BufNewFile,BufRead *.rkt set makeprg=racket\ %
autocmd BufNewFile,BufRead *.cr set makeprg=crystal\ %
autocmd BufNewFile,BufRead *.rs set makeprg=rustc\ %

" By default vim will indent arguments after the function name
" but sometimes you want to only indent by 2 spaces similar to
" how DrRacket indents define. Set the `lispwords' variable to
" add function names that should have this type of indenting.
set lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules

" automatically remove trailing characters
autocmd BufWritePre *.py,*.rb,*.sml,*.java,*.rkt,*.css,*.html,*.js,*.coffee,*.erb,*.haml  :%s/\s\+$//e

" code formatting
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set autoindent
set smartindent

" show bad characters
set listchars=tab:->,trail:·,extends:>,precedes:<
set list

" search
set incsearch
set hlsearch
set ignorecase

set keywordprg=trans\ :uk

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGIN OPTIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vundle support (plugin manager)
set nocompatible              " be iMproved, required
filetype off                  " required by Vundle

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'PotatoesMaster/i3-vim-syntax'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'ervandew/supertab'
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-airline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'elzr/vim-json'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'tpope/vim-obsession'
Plugin 'wlangstroth/vim-racket'
Plugin 'thoughtbot/vim-rspec'
Plugin 'vim-ruby/vim-ruby'
Plugin 'lucapette/vim-ruby-doc'
Plugin 'henrik/vim-ruby-runner'
Plugin 'chilicuil/vim-sml-coursera'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tpope/vim-git'
Plugin 'rust-lang/rust.vim'
Plugin 'timonv/vim-cargo'
Plugin 'tpope/vim-dispatch'
Plugin 'rhysd/vim-crystal'
Plugin 'Townk/vim-autoclose'
Plugin 'vim-scripts/HTML-AutoCloseTag'
Plugin 'kchmck/vim-coffee-script'
Plugin 'mattn/emmet-vim.git'
Plugin 'skammer/vim-css-color.git'
Plugin 'elixir-lang/vim-elixir'
Plugin 'mattreduce/vim-mix'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'wakatime/vim-wakatime'
Plugin 'rhysd/clever-f.vim'

call vundle#end()
filetype plugin indent on    " required by Vundle

" powerline
set laststatus=2
set t_Co=256

" airline
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_powerline_fonts = 1
let g:airline_theme='murmur'

" nerdtree
let NERDTreeMapJumpParent='h'
let NERDTreeMapActivateNode='l'
"autocmd VimEnter * NERDTree ../
autocmd VimEnter * wincmd p
let NERDTreeWinSize=20

" ctrlp
set runtimepath^=~/.vim/bundle/ctrlp.vim

" git gutter
nmap ]h <Plug>GitGutterNextHunk
nmap [h <Plug>GitGutterPrevHunk

nmap <Leader>hs <Plug>GitGutterStageHunk
nmap <Leader>hr <Plug>GitGutterRevertHunk

nmap <Leader>hp <Plug>GitGutterPreviewHunk

" Rainbow parentheses
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

"au VimEnter * RainbowParenthesesToggle
"autocmd Syntax * RainbowParenthesesLoadRound
"autocmd Syntax * RainbowParenthesesLoadSquare
"autocmd Syntax * RainbowParenthesesLoadBraces
autocmd BufNewFile,BufRead *.rkt,*.lisp :RainbowParenthesesToggle

" rspec
let g:rspec_command = "!rspec {spec} --format documentation"
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" vim colors solarized
syntax enable
set background=dark
colorscheme solarized
" and then use favourite colors
if has('gui_running')
  color codeschool
else
  color desert
endif

" easymotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
map <Leader>f <Plug>(easymotion-s)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap <Leader>f <Plug>(easymotion-s2)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" Vim-cargo
let g:cargo_command = "Dispatch cargo {cmd}"

" Emmet-vim
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

" Vim-markdown
let g:vim_markdown_folding_disabled=1
