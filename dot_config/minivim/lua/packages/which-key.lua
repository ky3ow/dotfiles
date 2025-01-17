local H = {}

function H.setup_visual_at()
	---@param reg string
	---@param content string
	local function create_register_keymap(reg, content)
		vim.keymap.set({ "x" }, "@" .. reg, ":normal @" .. reg .. "<CR>", { desc = content })
		vim.keymap.set({ "n" }, "@" .. reg, "@" .. reg, { desc = content })
	end

	local alphabet = vim.split("abcdefghijklmnopqrstuvwxyz", "")
	local macro_augroup = vim.api.nvim_create_augroup("ky3ow.Macros", { clear = true })

	vim.keymap.del("x", "@")
	vim.api.nvim_create_autocmd({ "VimEnter" }, {
		pattern = "*",
		group = macro_augroup,
		callback = function()
			for _, regname in ipairs(alphabet) do
				local content = vim.fn.getreg(regname)
				if content ~= "" then
					create_register_keymap(regname, content)
				end
			end
		end,
	})

	vim.api.nvim_create_autocmd({ "RecordingLeave" }, {
		pattern = "*",
		group = macro_augroup,
		callback = function()
			local regname = vim.v.event.regname
			local content = vim.v.event.regcontents
			if vim.list_contains(alphabet, regname) then
				create_register_keymap(regname, content)
			end
		end,
	})
end

local later = require("mini.deps").later

later(function()
	local miniclue = require "mini.clue"
	local clues = miniclue.gen_clues

	miniclue.setup {
		triggers = {
			{ mode = "n", keys = "<Leader>" }, { mode = "x", keys = "<Leader>" },
			{ mode = "n", keys = "@" }, { mode = "x", keys = "@" },
			{ mode = "n", keys = "[" }, { mode = "n", keys = "]" },
			{ mode = "i", keys = "<C-x>" },
			{ mode = "n", keys = "g" }, { mode = "x", keys = "g" },
			{ mode = "n", keys = "'" }, { mode = "n", keys = "`" },
			{ mode = "x", keys = "'" }, { mode = "x", keys = "`" },
			{ mode = "n", keys = '"' }, { mode = "x", keys = '"' },
			{ mode = "i", keys = "<C-r>" }, { mode = "c", keys = "<C-r>" },
			{ mode = "n", keys = "<C-w>" }, { mode = "t", keys = "<C-w>" },
			{ mode = "n", keys = "z" }, { mode = "x", keys = "z" },
		},

		window = {
			delay = 0,
			config = {
				width = 50,
			},
		},

		clues = {
			clues.builtin_completion(), clues.g(), clues.marks(),
			clues.registers { show_contents = true }, clues.windows(), clues.z(),
			{ mode = "n", keys = "<Leader>s", desc = "[S]earch" }, { mode = "n", keys = "<Leader>r", desc = "[R]eplace" },
			{ mode = "n", keys = "<Leader>l", desc = "[L]SP" }, { mode = "n", keys = "<Leader>w", desc = "[W]orkspace" },
			{ mode = "n", keys = "<Leader>sp", desc = "[S]earch [P]ackages" }
		},
	}

	H.setup_visual_at()
end)
