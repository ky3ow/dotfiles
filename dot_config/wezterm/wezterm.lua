wezterm = require 'wezterm'
Config = {}

require("keys")
require("colorscheme")

Config.use_fancy_tab_bar = false
Config.tab_bar_at_bottom = false
Config.command_palette_bg_color = "#303332"
Config.max_fps = 240
Config.tab_max_width = 30

return Config
