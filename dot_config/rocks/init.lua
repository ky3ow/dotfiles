-- [[ Leader ]]
vim.g.mapleader = " "
vim.g.maplocalleader = " "
-- [[ Options ]]
vim.wo.signcolumn = "yes"
vim.wo.relativenumber = true
vim.opt.hlsearch = true
vim.opt.mouse = "a"
vim.opt.tabstop = 4
vim.opt.gdefault = true
vim.opt.breakindent = true
vim.opt.undofile = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.termguicolors = true
vim.opt.foldmethod = "indent" -- indent by default, rewrite in treesitter configuration
vim.opt.foldlevel = 99 -- so file is not folded by default

-- [[ Plugin manager ]]
-- SHOULD BE FIRST
require("bootstrap").setup {}

-- [[ Colorscheme ]]
require("colors").setup {
		colorscheme = "duskfox"
}

-- [[ Filetypes assosiation ]]
require("filetypes").setup {}

-- [[ Basic Keymaps ]]
require("keymaps").setup {}
