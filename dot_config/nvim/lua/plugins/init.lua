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
		config = function()
			require("rose-pine").setup({
				styles = {
					italic = false,
				},
			})
			-- vim.cmd.colorscheme("rose-pine")
		end,
	},

	{
		"ellisonleao/gruvbox.nvim",
		priority = 1000, -- Ensure it loads first
		config = function()
			require("gruvbox").setup({
				undercurl = false,
				underline = false,
				bold = true,
				italic = {
					strings = false,
					emphasis = true,
					comments = false,
					operators = false,
					folds = false,
				},
			})
			--	vim.cmd.colorscheme("gruvbox")
		end,
	},

	{
		"folke/which-key.nvim",
		opts = {
			preset = "helix",
			spec = {
				{ "<leader>l", group = "[L]SP" },
				{ "<leader>s", group = "[S]earch" },
				{ "<leader>w", group = "[W]orkspace" },
			},
		},
	},

	{ -- Adds git related signs to the gutter, as well as utilities for managing changes
		"lewis6991/gitsigns.nvim",
		opts = {
			-- See `:help gitsigns.txt`
			signs = {
				add = { text = "+" },
				change = { text = "~" },
				delete = { text = "_" },
				topdelete = { text = "‾" },
				changedelete = { text = "~" },
			},
		},
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
		"akinsho/toggleterm.nvim",
		version = "*",
		config = function()
			require("toggleterm").setup({
				direction = "float",
				open_mapping = [[<c-\>]],
			})
			local Terminal = require("toggleterm.terminal").Terminal
			local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

			local function lazygit_toggle()
				lazygit:toggle()
			end

			vim.api.nvim_create_user_command("Lazygit", lazygit_toggle, { desc = "Lazygit ui" })

			vim.keymap.set("n", "<leader>g", lazygit_toggle, { desc = "[G]it" })
		end,
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
	{ "numToStr/Comment.nvim", opts = {} },
	"tpope/vim-sleuth",
}
