""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HOTKEYS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","

" custom usability hotkeys
" => general movement
nnoremap <leader>o jddko
inoremap <leader>o <ESC>jddko
nnoremap <leader>O kddO
inoremap <leader>O <ESC>kddO
nnoremap <leader>ee $
" => ruby & rails
" binding.pry
nnoremap <leader>br obinding.pry<ESC>
inoremap <leader>br binding.pry<ESC>
" erb tag
nnoremap <leader>et o<%=  %><ESC>F i
inoremap <leader>et <%=  %><ESC>F i
nnoremap <leader>er o<%  %><ESC>F i
inoremap <leader>er <%  %><ESC>F i
" end of custom usability hotkeys

" edit vimrc quickly
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>:echo 'configuration reloaded'<CR>

" quick quit
nnoremap <Space>q :q!<CR>

" tab navigation
nnoremap H gT
nnoremap L gt

" copy-paste bindings
vnoremap <C-c> "+yi
vnoremap <C-x> "+cse16
inoremap <C-v> <C-r><C-o>+

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
set noswapfile
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

" crp - copy relative path of %
" cap - copy absolute path of %
" cfn - copy file name of %
" cdn - copy directory name of %
if has("mac") || has("gui_macvim") || has("gui_mac")
  nnoremap <leader>crp :let @*=expand("%")<CR>
  nnoremap <leader>cap :let @*=expand("%:p")<CR>
  nnoremap <leader>cfn :let @*=expand("%:t")<CR>
  nnoremap <leader>cdn :let @*=expand("%:p:h")<CR>
elseif has("gui_gtk") || has("gui_gtk2") || has("gui_gnome") || has("unix")
  nnoremap <leader>crp :let @+=expand("%")<CR>
  nnoremap <leader>cap :let @+=expand("%:p")<CR>
  nnoremap <leader>cfn :let @+=expand("%:t")<CR>
  nnoremap <leader>cdn :let @+=expand("%:p:h")<CR>
endif

set splitbelow
set splitright

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
Plugin 'PotatoesMaster/i3-vim-syntax'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'ervandew/supertab'
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'altercation/vim-colors-solarized'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-obsession'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'
Plugin 'elzr/vim-json'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'wlangstroth/vim-racket'
Plugin 'lucapette/vim-ruby-doc'
Plugin 'henrik/vim-ruby-runner'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'timonv/vim-cargo'
Plugin 'rhysd/vim-crystal'
Plugin 'Townk/vim-autoclose'
Plugin 'vim-scripts/HTML-AutoCloseTag'
Plugin 'mattn/emmet-vim.git'
Plugin 'skammer/vim-css-color.git'
Plugin 'mattreduce/vim-mix'
Plugin 'godlygeek/tabular'
Plugin 'rhysd/clever-f.vim'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/neocomplcache.vim'
Plugin 'tpope/vim-rails'
Plugin 'dandorman/vim-colors'
Plugin 'vim-scripts/vim-auto-save'
Plugin 'justincampbell/vim-eighties'
Plugin 'sheerun/vim-polyglot'
Plugin 'kana/vim-operator-user'
Plugin 'haya14busa/vim-operator-flashy'
Plugin 'danro/rename.vim'
Plugin 'wesQ3/vim-windowswap'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mhinz/vim-startify'

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
nnoremap <leader>t :NERDTreeToggle<CR>
inoremap <leader>t <ESC>:NERDTreeToggle<CR>
let NERDTreeMapJumpParent='h'
let NERDTreeMapActivateNode='l'
let NERDTreeWinSize=20

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

autocmd BufNewFile,BufRead *.rkt,*.lisp :RainbowParenthesesToggle

" vim colors solarized
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
" move to {char}{char}
nmap ; <Plug>(easymotion-overwin-f2)
" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" Vim-cargo
let g:cargo_command = "Dispatch cargo {cmd}"

" Emmet-vim
let g:user_emmet_install_global = 1

" Vim-markdown
let g:vim_markdown_folding_disabled=1

" Ruby + Rails
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

" Unite
nnoremap <space>/ :Unite -no-empty -no-resize grep<cr>
nnoremap <space>s :Unite -quick-match buffer<cr>
nnoremap <space>c :UniteClose<cr>
" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " play nice with supertab
  let b:SuperTabDisabled=1
  " enable navigation
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)

  nmap <silent><buffer><expr> Enter unite#do_action('switch')
  nmap <silent><buffer><expr> <C-t> unite#do_action('tabswitch')
  nmap <silent><buffer><expr> <C-x> unite#do_action('splitswitch')
  nmap <silent><buffer><expr> <C-v> unite#do_action('vsplitswitch')

  imap <silent><buffer><expr> Enter unite#do_action('switch')
  imap <silent><buffer><expr> <C-t> unite#do_action('tabswitch')
  imap <silent><buffer><expr> <C-x> unite#do_action('splitswitch')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplitswitch')

  map <buffer> <C-p> <Plug>(unite_toggle_auto_preview)

  nnoremap <ESC> :UniteClose<cr>
endfunction

call unite#custom#profile('default', 'context', {
\   'direction': 'botright',
\   'vertical_preview': 1,
\   'winheight': 15
\ })

let g:unite_source_grep_max_candidates = 50

" Autosave
let g:auto_save = 1
let g:auto_save_silent = 1
let g:auto_save_in_insert_mode = 0

" Vim Operator flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$
let g:operator#flashy#flash_time = 200

" Ctrlp
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$|vendor\|\.hg$\|\.svn$\|\.doc\|public\/images\|public\/system\|data\|log\|tmp$\|humangrid$',
  \ 'file': '\.exe$\|\.so$\|\.dat$'
  \ }

" Startify
let g:startify_session_dir = '~/.vim/session'
let g:startify_enable_special         = 0
let g:startify_files_number           = 10
let g:startify_relative_path          = 1
let g:startify_change_to_dir          = 0
let g:startify_session_autoload       = 1
let g:startify_session_persistence    = 1
let g:startify_session_delete_buffers = 1
let g:startify_list_order = [
  \ ['   Recent files:'],
  \ 'files',
  \ ['   Recent files within this dir:'],
  \ 'dir',
  \ ['   Sessions:'],
  \ 'sessions',
  \ ['   Bookmarks:'],
  \ 'bookmarks',
\ ]
let g:startify_bookmarks = [
  \ {'n': '~/.vim/notes'},
  \ {'v': '~/.vimrc'},
  \ {'z': '~/.zshrc'},
  \ {'t': '~/.tmux.conf'}
\ ]
let g:startify_skiplist = [
  \ 'COMMIT_EDITMSG',
  \ escape(fnamemodify(resolve($VIMRUNTIME), ':p'), '\') .'doc',
  \ 'bundle/.*/doc',
\ ]
autocmd User Startified setlocal buftype=
let g:ctrlp_reuse_window = 'startify'
autocmd BufEnter * if @% == "" | :Startify | endif
