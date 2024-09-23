--  Vim Configuration
vim.cmd("set nocompatible")   -- move using arrow key
-- filtype plugin on
vim.cmd("set number")
vim.cmd("set ai") 	-- auto indent
vim.cmd("set si")  -- smart indent
vim.cmd("set cindent") --  c style indent

vim.cmd("set expandtab") -- tab to space
vim.cmd("set shiftwidth=4") -- shift order 4step
vim.cmd("set tabstop=4")    -- tab order 4step
vim.cmd("set hlsearch")	   -- highlight empathize the word.

vim.cmd("set history=1000")   -- history store depth
vim.cmd("set nobackup")   -- no generate swp file
vim.cmd("set noswapfile")
vim.cmd("set nowritebackup")
vim.cmd("set backupdir=~/.backup/")
vim.cmd("set directory=~/.backup/")

vim.cmd("set ruler")	   -- display the cursor position
vim.cmd("set title")	   -- display the title
vim.cmd("set showmatch")	   -- display the matched bracket
-- set nowrap	   "no auto linefeed
vim.cmd("set wmnu")	   -- auto word finder

vim.cmd("set autochdir")	--  auto change working directory

vim.cmd("set hidden") -- buffer hidden
vim.cmd("set updatetime=300") --  300ms
vim.cmd("set cmdheight=2")  --  Better display for messages
vim.cmd("set shortmess+=c")  --  don't give [ins-completion-menu] messages.
vim.cmd("set signcolumn=yes") --  always show signcolumns


vim.cmd("set backspace=indent,eol,start")
vim.cmd("set fencs=ucs-bom,utf-8,cp949")
-- vim.cmd("set clipboard^=unnamed,unnamedplus")
vim.cmd("set clipboard+=unnamedplus")

vim.cmd("set mouse=a")

-- normal
vim.keymap.set('n', '<C-h>', '<C-w>h', {})
vim.keymap.set('n', '<C-l>', '<C-w>l', {})
vim.keymap.set('n', '<C-j>', '<C-w>j', {})
vim.keymap.set('n', '<C-k>', '<C-w>k', {})
-- vim.keymap.set('n', '<C-c>', '"*y', {})
-- vim.keymap.set('n', '<C-n>', '"*p', {})
--
vim.keymap.set('n', '<F12>', ':cd %:p:h<cr>:term zsh<cr>', {})
vim.keymap.set('n', '<F7>', '@a', {})
