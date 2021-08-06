" Vim Configuration
set nocompatible   "move using arrow key
"filtype plugin on
set number
set ai 	"auto indent
set si  "smart indent
set cindent " c style indent

set shiftwidth=4 "shift order 4step
set tabstop=4    "tab order 4step
set hlsearch	   "highlight empathize the word.

set history=1000   "history store depth
set nobackup   "no generate swp file
set noswapfile
set nowritebackup
set backupdir=~/.backup/
set directory=~/.backup/

set ruler	   "display the cursor position
set title	   "display the title
set showmatch	   "display the matched bracket
"set nowrap	   "no auto linefeed
set wmnu	   "auto word finder

set autochdir	" auto change working directory

set hidden "buffer hidden
set updatetime=300 " 300ms
set cmdheight=2  " Better display for messages
set shortmess+=c  " don't give [ins-completion-menu] messages.
set signcolumn=yes " always show signcolumns


set backspace=indent,eol,start
set fencs=ucs-bom,utf-8,cp949
set clipboard^=unnamed,unnamedplus

set mouse=a

" PluginManager in vimrc
runtime plugList.vim

"""""""""""""""""""""""""""""""""""""""""""""""""""
" Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""" One Dark
"colorscheme onedark
"""""""""""""" Grub_box
"let g:gruvbox_contrast_dark='soft'
"set background=dark
"colorscheme gruvbox
"let g:airline_theme='gruvbox'
"""""""""""""" Paper Color
"set background=light
"colorscheme PaperColor
"let g:airline_theme='hybrid'
"let g:PaperColor_Theme_Options = {
"  \   'theme': {
"  \     'default.light': {
"  \       'transparent_background': 1
"  \     }
"  \   }
"  \ }
"""""""""""""" Molokai
"let g:molokai_original = 1
"colorscheme molokai
"""""""""""""" Solarized 8
"set background=light
"colorscheme solarized8_high
"let g:airline_theme='solarized8'
"""""""""""""" Challenger Deep
"colorscheme challenger_deep
colorscheme palenight

"""""""""""""""""""""""""""""""""""""""""""""""""""
" Air-line
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1 " turn on buffer list
set laststatus=2 " turn on bottom bar


""""""""""""""""""""""""""""""""""""""""""""""""""
" Git Gutter
""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_highlight_lines = 1
map <F4> :GitGutterLineHighlightsToggle<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""
" easymotion
""""""""""""""""""""""""""""""""""""""""""""""""""
" <Leader>f{char} to move to {char}
"map  <Leader>f <Plug>(easymotion-bd-f)
"nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap <Leader>s <Plug>(easymotion-overwin-f2)

" Move to line
"map <Leader>L <Plug>(easymotion-bd-jk)
"nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

map <C-c> "+ygv"*y
map <C-n> "+p
map <F12> :cd %:p:h<cr>:term bash<cr>
map <F7> @a
map <F8> "byaw/incNumber/b
cw"bp

""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual-star (Search using * / in visual modde)
""""""""""""""""""""""""""""""""""""""""""""""""""
xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>

function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction



