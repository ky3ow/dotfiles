-- [[ Options ]]
return {
	setup = function(_)
		vim.wo.signcolumn = "yes"
		vim.wo.relativenumber = true
		vim.wo.number = true
		vim.opt.updatetime = 250
		vim.opt.timeoutlen = 500 -- how long nvim waits on unresolved keybinds
		vim.opt.hlsearch = true
		vim.opt.mouse = "a"
		vim.opt.tabstop = 4
		vim.opt.gdefault = true
		vim.opt.breakindent = true
		vim.opt.undofile = true
		vim.opt.ignorecase = true
		vim.opt.smartcase = true
		vim.opt.termguicolors = true
		vim.opt.cursorline = true
		vim.opt.foldmethod = "indent" -- indent by default, rewrite in treesitter configuration
		vim.opt.foldlevel = 99 -- so file is not folded by default
		vim.opt.fillchars = [[fold: ]]
	end,
}
