MiniDeps.later(function()
	require("mini.comment").setup {
		options = {
			ignore_blank_line = true,
		},
	}
	require("mini.surround").setup {}
	require("mini.files").setup {
		options = {
			use_as_default_explorer = false,
		},
	}
	vim.keymap.set("n", "<leader>f", MiniFiles.open, { desc = "Mini [F]iles" })
end)

MiniDeps.now(function()
	MiniDeps.add "tpope/vim-fugitive"
	MiniDeps.add "prichrd/netrw.nvim"
	require("netrw").setup {}
	-- Use mini.git after getting hang of vim-fugitive
	-- require("mini.git").setup {}
	require("mini.statusline").setup {}
	require("mini.tabline").setup {}
end)

MiniDeps.later(function()
	MiniDeps.add "jinh0/eyeliner.nvim"
	require("eyeliner").setup {
		highlight_on_key = true,
		dim = true,
	}
	vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "DiagnosticError" })
	vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "DiagnosticInfo" })
end)

MiniDeps.later(function()
	MiniDeps.add "akinsho/toggleterm.nvim"
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
