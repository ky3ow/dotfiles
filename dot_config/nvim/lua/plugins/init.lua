return {
	{
		"folke/which-key.nvim",
		opts = {
			preset = "helix",
			delay = function(_) -- ctx.plugin
				return 0
			end,
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
			require("toggleterm").setup {
				direction = "float",
				open_mapping = [[<c-\>]],
			}
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
			require("nvim-surround").setup {
				-- Configuration here, or leave empty to use defaults
			}
		end,
	},

	{
		"jinh0/eyeliner.nvim",
		config = function()
			require("eyeliner").setup {
				highlight_on_key = true,
				dim = true,
			}
			vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "@keyword.return" })
			vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "@attribute" })
		end,
	},

	{ "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },
	{ "numToStr/Comment.nvim", opts = {} },
	"tpope/vim-sleuth",
}
