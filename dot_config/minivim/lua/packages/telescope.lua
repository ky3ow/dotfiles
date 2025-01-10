local add = require("mini.deps").add
local later = require("mini.deps").later

add {
	source = "nvim-telescope/telescope.nvim",
	checkout = "0.1.x",
	depends = {
		"nvim-telescope/telescope-ui-select.nvim",
		"nvim-lua/plenary.nvim",
		{ 
			source = "nvim-telescope/telescope-fzf-native.nvim",
			post_checkout = function(opts)
				if vim.fn.executable "make" ~= 1 then
					vim.notify("Fzf native: make not found", vim.log.levels.ERROR)
					return
				end
				vim.system({"make"}, { cwd = opts.path }):wait()
				print(vim.inspect(output))
			end
		},
	}
}

local function wrap(fn, config)
	return function()
		fn(config)
	end
end

later(function ()
	local actions = require "telescope.actions"
	local builtins = require "telescope.builtin"

	require("telescope").setup {
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
				require("telescope.themes").get_dropdown {},
			},
		},
	}
	pcall(require("telescope").load_extension, "fzf")
	pcall(require("telescope").load_extension, "ui-select")

	local ivy = require("telescope.themes").get_ivy {}
	local drop_noprev = require("telescope.themes").get_dropdown {
		winblend = 10,
		previewer = false,
	}

	vim.keymap.set(
		"n",
		"<leader>/",
		wrap(builtins.current_buffer_fuzzy_find, drop_noprev),
		{ desc = "[/] Fuzzily search in current buffer" }
	)

	vim.keymap.set("n", "<leader>sG", wrap(builtins.git_files, ivy), { desc = "[S]earch [G]it" })
	vim.keymap.set("n", "<leader>sf", wrap(builtins.find_files, ivy), { desc = "[S]earch [F]iles" })
	vim.keymap.set("n", "<leader>fp", wrap(builtins.find_files, vim.tbl_extend('force', ivy, {cwd = vim.g.mini_deps})), { desc = "[F]ind [P]ackages" })
	vim.keymap.set("n", "<leader>gp", wrap(builtins.live_grep, vim.tbl_extend('force', ivy, {cwd = vim.g.mini_deps})), { desc = "[G]rep [P]ackages" })
	vim.keymap.set("n", "<leader>sw", wrap(builtins.grep_string, ivy), { desc = "[S]earch current [W]ord" })
	vim.keymap.set("n", "<leader>sg", wrap(builtins.live_grep, ivy), { desc = "[S]earch by [G]rep" })

	vim.keymap.set("n", "<leader>sh", wrap(builtins.help_tags, ivy), { desc = "[S]earch [H]elp" })
	vim.keymap.set("n", "<leader>sd", wrap(builtins.diagnostics, ivy), { desc = "[S]earch [D]iagnostics" })
	vim.keymap.set("n", "<leader><leader>", wrap(builtins.buffers, ivy), { desc = "[ ] Find existing buffers" })
end)
