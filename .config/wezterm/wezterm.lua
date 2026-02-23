local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- Cross-platform font
config.font = wezterm.font_with_fallback({
	"Comic Shanns Mono Nerd Font",
	"Noto Color Emoji",
})
config.font_size = 12.0

-- Nord colors
config.colors = {
	foreground = "#D8DEE9",
	background = "#2E3440",
	cursor_bg = "#D8DEE9",
	cursor_fg = "#2E3440",
	selection_bg = "#4C566A",
	selection_fg = "#ECEFF4",
	ansi = {
		"#3B4252", -- black
		"#BF616A", -- red
		"#A3BE8C", -- green
		"#EBCB8B", -- yellow
		"#81A1C1", -- blue
		"#B48EAD", -- magenta
		"#88C0D0", -- cyan
		"#E5E9F0", -- white
	},
	brights = {
		"#4C566A", -- black
		"#BF616A", -- red
		"#A3BE8C", -- green
		"#EBCB8B", -- yellow
		"#81A1C1", -- blue
		"#B48EAD", -- magenta
		"#8FBCBB", -- cyan
		"#ECEFF4", -- white
	},
}

-- Window
config.window_background_opacity = 0.8
config.window_padding = { left = 6, right = 6, top = 6, bottom = 6 }
config.initial_cols = 100
config.initial_rows = 30
config.window_decorations = "NONE"

-- Scrollback
config.scrollback_lines = 10000

-- Cursor
config.default_cursor_style = "BlinkingBlock"
config.cursor_blink_rate = 0

-- Mouse
config.hide_mouse_cursor_when_typing = true

-- Tabs (simple)
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false

-- Bell
config.audible_bell = "Disabled"

-- Image protocol for yazi
config.enable_kitty_graphics = true

-- Keybindings
config.keys = {
	-- Copy/Paste
	{ key = "C", mods = "CTRL|SHIFT", action = wezterm.action.CopyTo("Clipboard") },
	{ key = "V", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom("Clipboard") },
	-- Font size
	{ key = "=", mods = "CTRL", action = wezterm.action.IncreaseFontSize },
	{ key = "-", mods = "CTRL", action = wezterm.action.DecreaseFontSize },
	{ key = "0", mods = "CTRL", action = wezterm.action.ResetFontSize },
}

return config
