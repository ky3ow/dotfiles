return {
	{
		"neanias/everforest-nvim",
		priority = 1000,
		config = function()
			local ef = require("everforest")
			ef.setup({
				background = "hard",
				disable_italic_comments = true,
			})
			ef.load()
		end,
	},
	{
		"rose-pine/neovim",
		name = "rose-pine",
		config = function ()
			-- require("rose-pine").setup {
			-- 	styles = {
			-- 		italic = false,
			-- 	},
			-- }
			-- vim.cmd.colorscheme("rose-pine")
		end
	},

	{
		"folke/which-key.nvim",
		config = function()
			local wk = require("which-key")
			wk.setup({})
			-- Normal
			wk.register({
				["<leader>g"] = { name = "[G]it", _ = "which_key_ignore" },
				["<leader>h"] = { name = "Git [H]unk", _ = "which_key_ignore" },
				["<leader>s"] = { name = "[S]earch", _ = "which_key_ignore" },
				["<leader>t"] = { name = "[T]oggle", _ = "which_key_ignore" },
				["<leader>w"] = { name = "[W]orkspace", _ = "which_key_ignore" },
				["<leader>l"] = { name = "[L]SP", _ = "which_key_ignore" },
			})
			-- Visual
			wk.register({
				["<leader>"] = { name = "VISUAL <leader>" },
				["<leader>h"] = { "Git [H]unk" },
			}, { mode = "v" })
		end,
		init = function()
			vim.o.updatetime = 250
			vim.o.timeoutlen = 150
		end,
	},

	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "auto",
				component_separators = "|",
				section_separators = "",
			},
		},
	},

	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		config = function()
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end,
	},
	{
		"jinh0/eyeliner.nvim",
		config = function()
			require("eyeliner").setup({
				highlight_on_key = true,
				dim = true,
			})
			vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "@text.danger" })
			vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "@text.warning" })
		end,
	},

	{ "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },
	{ "numToStr/Comment.nvim",               opts = {} },
	"tpope/vim-sleuth",
}
