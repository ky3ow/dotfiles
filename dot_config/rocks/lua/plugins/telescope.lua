local function wrap(fn, config)
		return function()
				fn(config)
		end
end

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
local telescope_ivy = function(fn)
		return wrap(fn, ivy)
end
local drop_noprev = require('telescope.themes').get_dropdown {
		winblend = 10,
		previewer = false,
}

vim.keymap.set("n", "<leader>/", wrap(builtins.current_buffer_fuzzy_find, drop_noprev),
{ desc = "[/] Fuzzily search in current buffer" })

vim.keymap.set("n", "<leader>sG", telescope_ivy(builtins.git_files), { desc = "[S]earch [G]it" })
vim.keymap.set("n", "<leader>sf", telescope_ivy(builtins.find_files), { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>sw", telescope_ivy(builtins.grep_string), { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>sg", telescope_ivy(builtins.live_grep), { desc = "[S]earch by [G]rep" })

vim.keymap.set("n", "<leader>sh", telescope_ivy(builtins.help_tags), { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sd", telescope_ivy(builtins.diagnostics), { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader><leader>", telescope_ivy(builtins.buffers), { desc = "[ ] Find existing buffers" })
