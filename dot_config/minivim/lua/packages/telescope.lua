local add = require("mini.deps").add
local now = require("mini.deps").now

local function wrap(fn, config)
	return function()
		fn(config)
	end
end

now(function()
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
					vim.system({ "make" }, { cwd = opts.path }):wait()
				end
			},
		}
	}

	local actions = require "telescope.actions"
	require("telescope").setup {}
	pcall(require("telescope").load_extension, "fzf")
end)

now(function()
	require("mini.pick").setup {
		mappings = {
			choose_marked = "<M-q>",
			refine_marked = "<M-r>",

			mark_down = {
				char = "<M-j>",
				func = function()
					local keys = vim.api.nvim_replace_termcodes("<C-x><C-n>", true, true, true)
					vim.api.nvim_feedkeys(keys, "n", false)
				end
			},
		},
	}
	require("mini.extra").setup {}

	local function with_package(fn)
		return function()
			fn(nil, { source = { cwd = vim.g.mini_deps } })
		end
	end

	vim.keymap.set("n", "<leader>ff", MiniPick.builtin.files, { desc = "[f]ind [f]iles" })
	vim.keymap.set("n", "<leader>gf", MiniPick.builtin.grep_live, { desc = "[g]rep [f]iles" })
	vim.keymap.set("n", "<leader>gg", MiniPick.builtin.grep, { desc = "[g]rep" })
	vim.keymap.set("n", "<leader>fh", MiniPick.builtin.help, { desc = "[f]ind [h]elp" })

	vim.keymap.set("n", "<leader>gc", function()
		MiniExtra.pickers.list { scope = "quickfix" }
	end, { desc = "[g]rep qui[c]fix" })

	vim.keymap.set("n", "<leader>fg", function()
		MiniExtra.pickers.git_files {
			scope = "modified"
		}
	end, { desc = "[f]ind [g]it" })

	vim.keymap.set("n", "<leader>fd", function()
			MiniExtra.pickers.diagnostic({}, { scope = "current" })
		end,
		{ desc = "[g]rep [p]ackages" })


	vim.keymap.set("n", "<leader>fp", with_package(MiniPick.builtin.files), { desc = "[f]ind [p]ackages" })
	vim.keymap.set("n", "<leader>gp", with_package(MiniPick.builtin.grep_live), { desc = "[g]rep [p]ackages" })

	vim.keymap.set("n", "<leader>gw", function()
			local word = vim.fn.expand("<cword>")
			MiniPick.builtin.grep({
				pattern = word
			}, {
				source = {
					name = string.format('Word: "%s"', word)
				},
			})
		end,
		{ desc = "[g]rep [w]ord" })

	vim.keymap.set("n", "<leader>/", function()
			MiniExtra.pickers.buf_lines({ scope = "current" })
		end,
		{ desc = "[/] fuzzy current buffer" })


	vim.ui.select = MiniPick.ui_select
end)
