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
	require("netrw").setup {} -- pretty netrw with icons
	-- Use mini.git after getting hang of vim-fugitive
	-- require("mini.git").setup {}
	require("mini.icons").setup {
		style = "glyph"
	}
	require("mini.statusline").setup {
		content = {
			active = function()
				local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
				local git           = MiniStatusline.section_git({ trunc_width = 40 })
				local diff          = MiniStatusline.section_diff({ trunc_width = 75 })
				local diagnostics   = MiniStatusline.section_diagnostics({ trunc_width = 75 })
				local lsp           = MiniStatusline.section_lsp({ trunc_width = 75 })
				local filename      = MiniStatusline.section_filename({ trunc_width = 140 })
				local fileinfo      = MiniStatusline.section_fileinfo({ trunc_width = 120 })
				local location      = MiniStatusline.section_location({ trunc_width = 75 })
				local search        = MiniStatusline.section_searchcount({ trunc_width = 75 })
				local yaml_schema   = (function(args)
					if MiniStatusline.is_truncated(args.trunc_width) then return '' end
					---@type SchemerYamlSchema
					local schema = vim.b.schemer_yaml_schema
					return schema and (schema.name or schema.uri) or ''
				end)({ trunc_width = 120 })

				return MiniStatusline.combine_groups({
					{ hl = mode_hl,                 strings = { mode } },
					{ hl = 'MiniStatuslineDevinfo', strings = { git, diff, diagnostics, lsp } },
					'%<', -- Mark general truncate point
					{ hl = 'MiniStatuslineFilename', strings = { filename } },
					'%=', -- End left alignment
					{ hl = 'MiniStatuslineFileinfo', strings = { yaml_schema, fileinfo } },
					{ hl = mode_hl,                  strings = { search, location } },
				})
			end,
			inactive = nil,
		},

		use_icons = true,
		set_vim_settings = true,
	}
	require("mini.tabline").setup {}
end)

MiniDeps.later(MiniIcons.tweak_lsp_kind)

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
