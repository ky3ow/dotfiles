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
		priority = 1000,
		name = "rose-pine",
		-- config = function()
		-- require("rose-pine").setup {
		-- 	styles = {
		-- 		italic = false,
		-- 	},
		-- }
		-- vim.cmd.colorscheme("rose-pine")
		-- end,
	},

	{
		"ellisonleao/gruvbox.nvim",
		priority = 1000, -- Ensure it loads first
		-- config = function()
		-- 	require("gruvbox").setup({
		-- 		undercurl = false,
		-- 		underline = false,
		-- 		bold = true,
		-- 		italic = {
		-- 			strings = false,
		-- 			emphasis = true,
		-- 			comments = false,
		-- 			operators = false,
		-- 			folds = false,
		-- 		},
		-- 	})
		-- 	vim.cmd.colorscheme("gruvbox")
		-- end,
	},

	{
		"folke/which-key.nvim",
		config = function()
			local wk = require("which-key")
			wk.setup({
				window = {
					border = "single",
					margin = { 1, 0, 2, 0.75 },
					padding = { 0, 0, 0, 0 },
				},
				layout = {
					height = { min = 4, max = 75 },
				},
			})
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
			vim.o.timeoutlen = 0
		end,
	},

	-- {
	-- 	"stevearc/oil.nvim",
	-- 	config = function()
	-- 		require("oil").setup({})
	-- 		vim.api.nvim_create_user_command("Ex", function(_)
	-- 			vim.cmd.Oil()
	-- 		end, { desc = "Open oil explorer" })
	-- 	end,
	-- },

	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "auto",
				component_separators = "|",
				section_separators = "",
			},
			sections = {
				lualine_c = { { "filename", path = 1 } },
			},
			tabline = {
				lualine_a = { { "buffers", use_mode_colors = true } },
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
