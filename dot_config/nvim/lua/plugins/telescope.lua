local function wrap(fn, config)
	return function()
		fn(config)
	end
end

local function setup()
	local actions = require("telescope.actions")
	local builtins = require("telescope.builtin")

	require("telescope").setup({
		defaults = {
			mappings = {
				i = {
					["<esc>"] = actions.close,
					["<C-u>"] = false,
					["<C-f>"] = actions.preview_scrolling_down,
					["<C-b>"] = actions.preview_scrolling_up,
				},
			},
		},
		extensions = {
			["ui-select"] = {
				require("telescope.themes").get_dropdown {}
			}
		}
	})
	pcall(require("telescope").load_extension, "fzf")
	pcall(require("telescope").load_extension, "ui-select")

	local ivy = require('telescope.themes').get_ivy {}
	local drop_nopreview = require('telescope.themes').get_dropdown {
		winblend = 10,
		previewer = false,
	}

	vim.keymap.set('n', '<leader>/', wrap(builtins.current_buffer_fuzzy_find, drop_nopreview),
		{ desc = '[/] Fuzzily search in current buffer' })

	vim.keymap.set("n", "<leader>sG", wrap(builtins.git_files, ivy), { desc = "[S]earch [G]it" })
	vim.keymap.set("n", "<leader>sf", wrap(builtins.find_files, ivy), { desc = "[S]earch [F]iles" })
	vim.keymap.set("n", "<leader>sw", wrap(builtins.grep_string, ivy), { desc = "[S]earch current [W]ord" })
	vim.keymap.set("n", "<leader>sg", wrap(builtins.live_grep, ivy), { desc = "[S]earch by [G]rep" })

	vim.keymap.set("n", "<leader>sh", wrap(builtins.help_tags, ivy), { desc = "[S]earch [H]elp" })
	vim.keymap.set("n", "<leader>sd", wrap(builtins.diagnostics, ivy), { desc = "[S]earch [D]iagnostics" })
	vim.keymap.set("n", "<leader><space>", wrap(builtins.buffers, ivy), { desc = "[ ] Find existing buffers" })
end

return {
	{
		"nvim-telescope/telescope.nvim",
		config = setup,
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make", -- make is installed on system
				cond = function()
					return vim.fn.executable("make") == 1
				end,
			},
		},
	},
}
