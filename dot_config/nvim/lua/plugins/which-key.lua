local function setup_buffers()
	local function create_goto_keymaps(excluded_buf)
		local buffers = vim.fn.getbufinfo({ buflisted = 1 })
		local filtered_buffers = {}
		for _, buf in ipairs(buffers) do
			if buf.bufnr ~= excluded_buf then
				table.insert(filtered_buffers, buf)
			end
		end
		for i = 1, 9 do
			local buffer = filtered_buffers[i] or { name = "" }
			local name = vim.fn.fnamemodify(buffer.name, ":p:.")

			vim.api.nvim_set_keymap(
				"n",
				"<leader>" .. i,
				"<cmd>silent! buffer " .. buffer.name .. "<cr>",
				{ noremap = true, silent = true, desc = "Go to: " .. (name:len() > 0 and name or "-") }
			)
		end
	end

	vim.api.nvim_create_autocmd({ "BufDelete", "BufAdd" }, {
		pattern = "*",
		callback = function(e)
			create_goto_keymaps(e.event == "BufDelete" and e.buf or -1)
		end,
	})
end

local function setup_visual_at()
	local alphabet = vim.split("abcdefghijklmnopqrstuvwxyz", "")

	vim.keymap.del("x", "@")
	vim.api.nvim_create_autocmd({ "VimEnter" }, {
		pattern = "*",
		callback = function()
			for _, reg in ipairs(alphabet) do
				local content = vim.fn.getreg(reg)
				if content ~= "" then
					vim.keymap.set({ "x" }, "@" .. reg, ":normal @" .. reg .. "<CR>", { desc = content })
					vim.keymap.set({ "n" }, "@" .. reg, "@" .. reg, { desc = content })
				end
			end
		end,
	})

	vim.api.nvim_create_autocmd({ "RecordingLeave" }, {
		pattern = "*",
		callback = function()
			local regname = vim.v.event.regname
			local content = vim.v.event.regcontents
			if vim.list_contains(alphabet, regname) then
				vim.keymap.set({ "x" }, "@" .. regname, ":normal @" .. regname .. "<CR>", { desc = content })
				vim.keymap.set({ "n" }, "@" .. regname, "@" .. regname, { desc = content })
			end
		end,
	})
end

return {

	-- {
	-- 	"folke/which-key.nvim",
	-- 	config = function()
	-- 		local wk = require("which-key")
	-- 		wk.setup({
	-- 			preset = "helix",
	-- 			delay = function(_) -- ctx.plugin
	-- 				return 0
	-- 			end,
	-- 			spec = {
	-- 				{ "<leader>l", group = "[L]SP" },
	-- 				{ "<leader>s", group = "[S]earch" },
	-- 				{ "<leader>w", group = "[W]orkspace" },
	-- 			},
	-- 		})
	--
	-- 		setup_buffers()
	-- 		setup_visual_at()
	-- 	end,
	-- },

	{
		"echasnovski/mini.nvim",
		verison = false,
		config = function()
			local miniclue = require("mini.clue")
			miniclue.setup({
				triggers = {
					-- Leader triggers
					{ mode = "n", keys = "<Leader>" },
					{ mode = "x", keys = "<Leader>" },

					-- Macro
					{ mode = "n", keys = "@" },
					{ mode = "x", keys = "@" },

					{ mode = "n", keys = "[" },
					{ mode = "n", keys = "]" },

					-- Built-in completion
					{ mode = "i", keys = "<C-x>" },

					-- `g` key
					{ mode = "n", keys = "g" },
					{ mode = "x", keys = "g" },

					-- Marks
					{ mode = "n", keys = "'" },
					{ mode = "n", keys = "`" },
					{ mode = "x", keys = "'" },
					{ mode = "x", keys = "`" },

					-- Registers
					{ mode = "n", keys = '"' },
					{ mode = "x", keys = '"' },
					{ mode = "i", keys = "<C-r>" },
					{ mode = "c", keys = "<C-r>" },

					-- Window commands
					{ mode = "n", keys = "<C-w>" },

					-- `z` key
					{ mode = "n", keys = "z" },
					{ mode = "x", keys = "z" },
				},

				window = {
					delay = 0,
					config = {
						width = 50,
					},
				},

				clues = {
					miniclue.gen_clues.builtin_completion(),
					miniclue.gen_clues.g(),
					miniclue.gen_clues.marks(),
					miniclue.gen_clues.registers({ show_contents = true }),
					miniclue.gen_clues.windows(),
					miniclue.gen_clues.z(),
					{ mode = "n", keys = "<Leader>s", desc = "[S]earch" },
					{ mode = "n", keys = "<Leader>r", desc = "[R]eplace" },
					{ mode = "n", keys = "<Leader>l", desc = "[L]SP" },
					{ mode = "n", keys = "<Leader>w", desc = "[W]orkspace" },
				},
			})

			setup_buffers()
			setup_visual_at()
		end,
	},

}
