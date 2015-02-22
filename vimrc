""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HOTKEYS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","

" copy-paste bindings
vnoremap <C-c> "+yi
vnoremap <C-x> "+cse16
inoremap <C-v> <C-r><C-o>+

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

" useful combinations in insert mode
inoremap II <Esc>I
inoremap AA <Esc>A
inoremap OO <Esc>o
inoremap CC <Esc>C
inoremap DD <Esc>S
inoremap UU <Esc>u

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

" syntax and filetypes
syntax   on
filetype on
filetype indent on
filetype plugin on

" colors
colorscheme desert
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

" By default vim will indent arguments after the function name
" but sometimes you want to only indent by 2 spaces similar to
" how DrRacket indents define. Set the `lispwords' variable to
" add function names that should have this type of indenting.
set lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules

" automatically remove trailing characters
autocmd BufWritePre *.py,*.rb,*.sml,*.java,*.rkt :%s/\s\+$//e

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
" pathogen support (plugin installer)
execute pathogen#infect()

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
let g:rspec_command = "!rspec {spec}"
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>
