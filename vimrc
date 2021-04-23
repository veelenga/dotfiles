""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HOTKEYS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","

" => general mappings
nnoremap <leader>o jS
inoremap <leader>o <ESC>jS
nnoremap <leader>O kS
inoremap <leader>O <ESC>kS
nnoremap <leader>ee $
nnoremap <leader>bb ^
nnoremap <D-j> :m .+1<CR>==
nnoremap <D-k> :m .-2<CR>==
" spelling quick fix
nnoremap <leader>fs 1z=

" => ruby & rails
" binding.pry
nnoremap <leader>br obinding.pry<ESC>
inoremap <leader>br binding.pry<ESC>
" erb tag
nnoremap <leader>et o<%=  %><ESC>F i
inoremap <leader>et <%=  %><ESC>F i
nnoremap <leader>er o<%  %><ESC>F i
inoremap <leader>er <%  %><ESC>F i

" special files
nnoremap <leader>sv :source $MYVIMRC<CR>:echo 'configuration reloaded'<CR>
nnoremap <leader>v  :vsplit $MYVIMRC<CR>
nnoremap <leader>n  :vsplit ~/.vim/notes<CR>
nnoremap <leader>s  :vsplit ~/.vim/snippets<CR>

" quick quit
nnoremap <Space>q :q!<CR>
nnoremap <Space>w :call QuitTab()<CR>
function! QuitTab()
  try
    tabclose!
  catch /E784/ " Can't close last tab
    qall
  endtry
endfunction

" tab navigation
nnoremap H gT
nnoremap L gt

" copy-paste bindings
if has("gui_gtk") || has("gui_gtk2") || has("gui_gnome") || has("unix")
  vnoremap <C-c> "+yi
  vnoremap <C-x> "+cse15
  inoremap <C-v> <C-r><C-o>+
endif

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
map <F1> :RainbowParenthesesToggle<CR>
map <F2> :setlocal spell! spelllang=en_us<CR>
map <F8> :TagbarToggle<CR>

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
  map - 4<C-W><
  map = 4<C-W>>
endif

" Feature branch commit message
function! CommitPrefix(name)
  if (strlen(getline(".")))
    return
  endif

  let branch = system("git rev-parse --abbrev-ref HEAD")
  let match = matchstr(branch, a:name . '\([a-z]\+-[0-9]\+\)')
  if strlen(match)
    let ticket = toupper(substitute(match, a:name, '', 'g'))
    exe "normal O" . ticket . " " | startinsert
  endif
endfunction

au FileType gitcommit au! BufEnter COMMIT_EDITMSG call CommitPrefix('ve-')

" crp - copy relative path of %
" cap - copy absolute path of %
" cfn - copy file name of %
" cdn - copy directory name of %
nnoremap <leader>crp :let @*=expand("%").':'.line(".")<CR>:echo(@*)<CR>
nnoremap <leader>cap :let @*=expand("%:p").':'.line(".")<CR>:echo(@*)<CR>
nnoremap <leader>cfn :let @*=expand("%:t").':'.line(".")<CR>:echo(@*)<CR>
nnoremap <leader>cdn :let @*=expand("%:p:h")<CR>:echo(@*)<CR>

map q: :q

vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
nnoremap <Tab> >>
nnoremap <S-Tab> <<

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
set relativenumber
set number

" save automatically
set autowriteall

" reload file automatically
set autoread

set path+=**
set wildmenu

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
" autocmd BufWritePre *.py,*.rb,*.sml,*.java,*.rkt,*.css,*.html,*.js,*.coffee,*.erb,*.haml :%s/\s\+$//e
" http://vimcasts.org/episodes/tidying-whitespace/
function! StripTrailingWhitespaces()
  "Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

" nnoremap <leader>sw :call StripTrailingWhitespaces()<CR>

" code formatting
set noeol
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

set splitbelow
"set splitleft

set foldenable
set conceallevel=0

" netrw settings
augroup netrw_mapping
    autocmd!
    autocmd filetype netrw call NetrwMapping()
augroup END

function! NetrwMapping()
    map <buffer> l <Enter>
    map <buffer> h -
endfunction

" do not display info on the top of window
let g:netrw_banner = 0

set undofile
set undodir=~/.vim/undo/

set complete-=i

set exrc

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGIN OPTIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible " be iMproved, required
filetype off     " required by Vundle

" set the runtime path to include Vundle and initialize
" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
  execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin()

Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Lokaltog/vim-easymotion'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'airblade/vim-gitgutter'
Plug 'christoomey/vim-tmux-navigator'
Plug 'jiangmiao/auto-pairs'
Plug 'alvan/vim-closetag'
Plug 'Shougo/unite.vim'
Plug 'tpope/vim-rails'
Plug 'vim-ruby/vim-ruby'
Plug 'flazz/vim-colorschemes'
Plug '907th/vim-auto-save'
Plug 'justincampbell/vim-eighties'
Plug 'sheerun/vim-polyglot'
Plug 'danro/rename.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'mhinz/vim-startify'
Plug 'itchyny/vim-cursorword'
Plug 'scrooloose/nerdcommenter'
Plug 'kana/vim-textobj-user'
Plug 'yggdroot/indentline'
"Plug 'Valloric/YouCompleteMe'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'vim-syntastic/syntastic'
Plug 'ramele/agrep'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'isaacsloan/vim-slang'
Plug 'rhysd/vim-crystal'

call plug#end()
filetype plugin indent on    " required by Vundle

" Colorschemes
if has('gui_running')
  color codeschool
else
  color desert
endif

" Powerline
set laststatus=2
set t_Co=256

" Airline
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_powerline_fonts = 1
let g:airline_theme='murmur'

" Nerdtree
map <leader>fd :NERDTreeFind<cr>
nnoremap <leader>t :NERDTreeToggle<CR>
inoremap <leader>t <ESC>:NERDTreeToggle<CR>
let NERDTreeMapJumpParent='h'
let NERDTreeMapActivateNode='l'
let NERDTreeWinSize=30
" https://github.com/scrooloose/nerdtree/issues/21
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Git gutter
nmap ]h <Plug>GitGutterNextHunk
nmap [h <Plug>GitGutterPrevHunk
nmap <Leader>hs <Plug>GitGutterStageHunk
nmap <Leader>hr <Plug>GitGutterRevertHunk
nmap <Leader>hp <Plug>GitGutterPreviewHunk

" Easymotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings
nmap ; <Plug>(easymotion-overwin-f2)
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
" turn on case insensitive feature
let g:EasyMotion_smartcase = 1
" jk motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" Ruby + Rails
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
let g:ruby_indent_block_style = 'do'
let g:ruby_indent_assignment_style = 'variable'
let g:ruby_indent_access_modifier_style = 'indent'

" Unite
nnoremap <space>/ :Unite -no-empty -no-resize grep<cr>
nnoremap <space>s :Unite -quick-match buffer<cr>
nnoremap <space>c :UniteClose<cr>
" custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
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
  \ 'direction': 'botright',
  \ 'vertical_preview': 1,
  \ 'winheight': 15
\ })

let g:unite_source_grep_max_candidates = 50

" Autosave
" https://github.com/907th/vim-auto-save
let g:auto_save = 1
let g:auto_save_silent = 1
let g:auto_save_in_insert_mode = 0
set updatetime=500
let g:auto_save_events = ['CursorHold', 'InsertLeave', 'CompleteDone']

" Ctrlp
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|doc$\|public$\|log$\|tmp$\|upload$\|uploads$',
  \ 'file': '\.exe$\|\.so$\|\.dat$'
\ }

let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

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
let g:ctrlp_reuse_window = 'startify'
nnoremap  <leader>st :Startify<CR>
autocmd User Startified set buftype=

" Clever-f.vim
let g:clever_f_fix_key_direction = 1
let g:clever_f_show_prompt = 1
let g:clever_f_chars_match_any_signs = ';'

" Vim textobj user https://github.com/whatyouhide/vim-textobj-erb
let s:whitespace = '\(\s\|\n\)*'
let s:left_modifiers = '\(-\|=\{1,2}\|#\)\?'
let s:right_modifiers = '\-\?'
let s:left = '<%' . s:left_modifiers . s:whitespace
let s:right = s:whitespace . s:right_modifiers . '%>'
call textobj#user#plugin('erb', {
\   '-': {
\     'pattern': [s:left, s:right],
\     'select-a': 'aE',
\     'select-i': 'iE'
\   },
\ })

" Vim-textobj-rubyblock
runtime macros/matchit.vim

" Vim-markdown
let g:vim_markdown_conceal = 0
let g:vim_markdown_emphasis_multiline = 0
let g:vim_markdown_folding_level = 2

" YouCompleteMe
"autocmd CompleteDone * pclose
set completeopt-=preview
set shortmess+=c
let g:ycm_register_as_syntastic_checker = 0
let g:clang_debug = 0

" Ultisnips
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger = "<nop>"
let g:ulti_expand_or_jump_res = 0
let g:endwise_no_mappings = 1
function! ExpandSnippetOrCarriageReturn()
    let snippet = UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res > 0
        return snippet
    else
        return "\<CR>"
    endif
endfunction
inoremap <expr> <CR> pumvisible() ? "\<C-R>=ExpandSnippetOrCarriageReturn()\<CR>" : "\<CR>\<C-R>=EndwiseDiscretionary()\<CR>"
let g:UltiSnipsJumpForwardTrigger='<c-b>'
let g:UltiSnipsJumpBackwardTrigger='<c-z>'
let g:UltiSnipsJumpForwardTrigger='<c-j>'
let g:UltiSnipsJumpBackwardTrigger='<c-k>'
inoremap <c-x><c-k> <c-x><c-k>

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" Agrep
let agrep_default_flags = '-I --exclude-dir={.git,log,tmp}'
if !exists('s:agrep_cmd')
  au BufWinEnter Agrep setlocal nornu | setlocal nowrap
  let s:agrep_cmd = 1
  "nnoremap <ESC> :Aclose<cr>
endif
nnoremap <leader>gg :Agrep -r '
nnoremap <leader>gw :Agrep -r '<cword>' .
let g:agrep_results_win_sp_mod = 'vs'

" Auto-pair
let g:AutoPairsShortcutBackInsert = '<C-b>'

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" let g:syntastic_enable_sign = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_javascript_checkers = ['jshint']
" let g:syntastic_crystal_checkers = ['ameba']

" Closetag
let g:closetag_filenames = "*.html,*.xhtml,*.html.erb"

" Indentline
let g:indentLine_faster = 1
