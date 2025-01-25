-- Do not setup lazydev multiple times
vim.opt.tabstop = 4
vim.opt.formatoptions:remove "o" -- `o` don't add comment

if vim.g.lua_loaded then
	return
end
vim.g.lua_loaded = true

require("lazydev").setup {
	library = {
		-- See the configuration section for more details
		-- Load luvit types when the `vim.uv` word is found
		{ path = "luvit-meta/library", words = { "vim%.uv" } },
	},
}
