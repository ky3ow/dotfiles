return {
	{
		"folke/which-key.nvim",
		config = function()
			local wk = require("which-key")
			wk.setup({
				preset = "helix",
				delay = function(_) -- ctx.plugin
					return 0
				end,
				spec = {
					{ "<leader>l", group = "[L]SP" },
					{ "<leader>s", group = "[S]earch" },
					{ "<leader>w", group = "[W]orkspace" },
				},
			})

			local lualine_buffers = require("lualine.components.buffers")
			local function create_goto_keymap(number)
				return {
					"<leader>" .. number,
					function()
						lualine_buffers.buffer_jump(number, "!")
					end,
					desc = function()
						local actual_bufnr = lualine_buffers.bufpos2nr[number]
						local bufname = actual_bufnr and vim.api.nvim_buf_get_name(actual_bufnr) or "-"
						-- :h filename-modifiers
						-- full path, reduced relative to current directory
						local name = vim.fn.fnamemodify(bufname, ":p:.")
						-- name = vim.fn.pathshorten(name)
						return "Go to " .. name
					end,
				}
			end

			for i = 1, 9 do
				wk.add(create_goto_keymap(i))
			end

			vim.keymap.del("x", "@") -- delete default mapping
			wk.add({
				"@",
				group = "M[@]cro over selection",
				mode = { "x", "n" },
				expand = function()
					local registers = require("which-key.plugins.registers").expand()

					local specs = {} --- @type wk.Spec[]
					for _, reg in ipairs(registers) do
						local bytes = string.byte(reg.key)
						if bytes >= 97 and bytes <= 122 then
							local spec = { --- @type wk.Spec
								reg.key,
								function()
									local cmd = vim.api.nvim_replace_termcodes(
										":normal @" .. reg.key .. "<CR>",
										true,
										true,
										true
									)
									vim.api.nvim_feedkeys(cmd, "n", false)
								end,
								desc = reg.value,
							}
							table.insert(specs, spec)
						end
					end
					return specs
				end,
			})
		end,
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
