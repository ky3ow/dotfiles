local add = require("mini.deps").add
local now, later = require("mini.deps").now, require("mini.deps").later

later(function()
	add "lewis6991/gitsigns.nvim"
	require("gitsigns").setup {
		-- See `:help gitsigns.txt`
		signs = {
			add = { text = "+" },
			change = { text = "~" },
			delete = { text = "_" },
			topdelete = { text = "‾" },
			changedelete = { text = "~" },
		},
	}
end)

now(function()
	add "tpope/vim-sleuth"
	add "nvim-lualine/lualine.nvim"
	require("lualine").setup {
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
	}
end)

later(function()
	add "jinh0/eyeliner.nvim"
	require("eyeliner").setup {
		highlight_on_key = true,
		dim = true,
	}
	vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "@keyword.return" })
	vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "@attribute" })
end)

later(function()
	add "numToStr/Comment.nvim"
	require("Comment").setup {}
end)

later(function()
	add "kylechui/nvim-surround"
	require("nvim-surround").setup {}
end)

later(function()
	add "akinsho/toggleterm.nvim"
	vim.api.nvim_create_autocmd("TermOpen", {
		pattern = "term://*",
		callback = function(_)
			local opts = { buffer = 0 }

			vim.keymap.set("t", "<C-w><esc>", [[<C-\><C-n>]], opts)
			vim.keymap.set("t", "<C-w>.", "<C-w>", opts)
			for _, value in ipairs { "h", "j", "k", "l" } do
				vim.keymap.set("t", "<C-w>" .. value, "<C-\\><C-n>:wincmd " .. value .. "<CR>", opts)
			end
		end,
	})

	require("toggleterm").setup {
		direction = "horizontal",
		open_mapping = [[<c-\>]],
		persist_mode = false,
	}

	local Terminal = require("toggleterm.terminal").Terminal
	local lazygit = Terminal:new { cmd = "lazygit", hidden = true, direction = "float" }

	local function lazygit_toggle()
		lazygit:toggle()
	end

	vim.api.nvim_create_user_command("Lazygit", lazygit_toggle, { desc = "Lazygit ui" })

	vim.keymap.set("n", "<leader>G", lazygit_toggle, { desc = "[G]it" })
end)
