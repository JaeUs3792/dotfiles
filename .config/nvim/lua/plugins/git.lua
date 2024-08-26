return {
	{
		"tpope/vim-fugitive",  -- git wrapper
		config = function()
		end
	},		
	{
		"airblade/vim-gitgutter",  -- git modified view;
		config = function()
			vim.g.gitgutter_highlight_lines = 1
			vim.keymap.set({'n', 'v'}, '<F4>', ':GitGutterLineHighlightsToggle<cr>', {})
		end
	},		
}
