require("gvfs"):setup({
  which_keys = "1234567890qwertyuiopasdfghjklzxcvbnm",
  blacklist_devices = { { scheme = "file" } },
})

local home = os.getenv("HOME")
require("yamb"):setup({
  bookmarks = {
    { tag = "Home",      path = home .. "/",           key = "h" },
    { tag = "Downloads", path = home .. "/Downloads/", key = "d" },
    { tag = "dotfiles",  path = home .. "/.dotfiles/", key = "." },
  },
  jump_notify = true,
  cli = "fzf",
  keys = "0123456789abcdefghijklmnopqrstuvwxyz",
  path = home .. "/.config/yazi/bookmark",
})
