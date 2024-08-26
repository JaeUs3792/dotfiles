return { 
	{
		"tpope/vim-unimpaired",
		config = function()
		end
	},
	{
		"tpope/vim-commentary",
		config = function()
		end
	},
	{
		"tpope/vim-surround",
		config = function()
		end
	},
	{
		"easymotion/vim-easymotion",
		config = function()
			vim.keymap.set('n', '<Leader>s', '<Plug>(easymotion-overwin-f2)', {})
			vim.keymap.set('n', '<Leader>w', '<Plug>(easymotion-bd-w)', {})
			-- vim.keymap.set('n', '<Leader>w', '<Plug>(easymotion-bd-w)', {})
		end
	},
}
