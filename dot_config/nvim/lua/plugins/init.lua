return {
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
			vim.api.nvim_create_autocmd("TermOpen", {
				pattern = "term://*",
				callback = function(_)
					local opts = { buffer = 0 }

					vim.keymap.set("t", "<C-w><esc>", [[<C-\><C-n>]], opts)
					vim.keymap.set("t", "<C-w>]", [[<C-\><C-n>]], opts)
					vim.keymap.set("t", "<C-w>.", "<C-w>", opts)
					for _, value in ipairs({ "h", "j", "k", "l" }) do
						vim.keymap.set("t", "<C-w>" .. value, "<C-\\><C-n>:wincmd " .. value .. "<CR>", opts)
					end
				end,
			})

			require("toggleterm").setup({
				direction = "horizontal",
				open_mapping = [[<c-\>]],
				persist_mode = false,
			})

			local Terminal = require("toggleterm.terminal").Terminal
			local lazygit = Terminal:new({ cmd = "lazygit", hidden = true, direction = "float" })

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
			vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "@keyword.return" })
			vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "@attribute" })
		end,
	},

	{ "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },
	{ "numToStr/Comment.nvim", opts = {} },
	"tpope/vim-sleuth",
}
