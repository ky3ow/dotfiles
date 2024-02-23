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
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- [[ Plugins ]] from lua/plugins directory
require("lazy").setup("plugins")

-- [[ Highlight on yank ]]
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = "*",
})

-- [[ Basic Keymaps ]]

vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set("n", "<C-e>", "<Nop>")

vim.keymap.set("n", "<esc>", vim.cmd.nohlsearch, { desc = "Clear highlight" })

vim.keymap.set("n", "<leader>e", vim.cmd.Ex, { desc = "[E]xplorer" })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set("n", "<leader>d", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- Center jumping
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up" })
vim.keymap.set("n", "n", "nzzzv", { desc = "Next search" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Prev search" })

-- Navigate buffers
vim.keymap.set("n", "]b", vim.cmd.bnext, { desc = "Next [B]uffer" })
vim.keymap.set("n", "[b", vim.cmd.bprevious, { desc = "Prev [B]uffer" })

-- Navigate tabs
vim.keymap.set("n", "]t", "gt", { desc = "Next [T]ab" })
vim.keymap.set("n", "[t", "gT", { desc = "Prev [T]ab" })

-- Stay in indent mode
vim.keymap.set("v", "<", "<gv", { desc = "Outdent" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent" })

-- Move text
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move down" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move up" })

-- Magic with registers
vim.keymap.set({ "n", "v" }, "<leader>y", '"+y', { desc = "Copy to clip" })
vim.keymap.set("n", "<leader>Y", '"+Y', { desc = "Copy end to clip" })
vim.keymap.set("v", "<leader>p", '"_dP', { desc = "Paste over" })
vim.keymap.set("v", "<leader>d", '"_d', { desc = "Delete" })

-- Substitute
vim.keymap.set("n", "<leader>*", ":%s/<C-r><C-w>//gI<Left><Left><Left>", { desc = "Replace w[*]rd" })
vim.keymap.set("n", "<leader>_", ":%s/\\<<C-r><C-w>\\>//gI<Left><Left><Left>", { desc = "Replace whole[_] word" })
vim.keymap.set("n", "<leader>sr", [[:%s/\v]], { desc = "[S]earch and [r]eplace" })
vim.keymap.set("v", "s", [[:s/\v]], { desc = "Substitute" })
