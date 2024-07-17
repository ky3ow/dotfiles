-- [[ Leader ]]
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("config.options").setup {}
require("config.filetypes").setup {}
require("config.keymaps").setup {}

require("config.lazy").setup {}
