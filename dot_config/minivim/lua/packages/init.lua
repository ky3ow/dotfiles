MiniDeps.later(function()
	-- TODO! quicker-nvim, nvim-bqf
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
	vim.keymap.set("n", "<leader>f", MiniFiles.open, { desc = "[F]iles" })

	MiniDeps.add "tpope/vim-fugitive"

	MiniDeps.add "samiulsami/fFtT-highlights.nvim"
	require "fFtT-highlights":setup {
		jumpable_chars = {
			show_instantly_jumpable = "on_key_press",
			show_secondary_jumpable = "on_key_press"
		}
	}
	vim.api.nvim_set_hl(0, "fFtTUniqueHighlight", { link = "DiagnosticError" })
	vim.api.nvim_set_hl(0, "fFtTUniqueHighlightSecondary", { link = "DiagnosticInfo" })

	MiniIcons.tweak_lsp_kind()

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

	MiniDeps.add {
		source = "jake-stewart/multicursor.nvim",
		checkout = "1.0",
		monitor = "main",
	}

	local mc = require "multicursor-nvim"
	local wrap = function(func, arg)
		return function()
			func(arg)
		end
	end
	mc.setup()

	vim.keymap.set({ "n", "x" }, "<A-j>", wrap(mc.lineAddCursor, 1))
	vim.keymap.set({ "n", "x" }, "<A-k>", wrap(mc.lineAddCursor, -1))
	vim.keymap.set({ "n", "x" }, "<A-q>", mc.toggleCursor)
	vim.keymap.set({ "n", "x" }, "<A-*>", wrap(mc.matchAddCursor, 1))
	vim.keymap.set({ "n", "x" }, "<A-z>", wrap(mc.duplicateCursors, 1))

	vim.keymap.set({ "n", "x" }, "<A-m>", mc.operator)
	vim.keymap.set({ "n", "x" }, "gz", mc.restoreCursors)

	vim.keymap.set("x", "S", mc.matchCursors)
	vim.keymap.set("x", "<A-s>", mc.splitCursors)
	vim.keymap.set("n", "&", mc.alignCursors)
	vim.keymap.set("x", "I", mc.insertVisual)
	vim.keymap.set("x", "A", mc.appendVisual)

	vim.keymap.set({ "n", "x" }, "g<c-a>", mc.sequenceIncrement)
	vim.keymap.set({ "n", "x" }, "g<c-x>", mc.sequenceDecrement)

	vim.keymap.set("n", "<A-n>", wrap(mc.searchAddCursor, 1))
	vim.keymap.set("n", "<A-N>", wrap(mc.searchAddCursor, -1))

	mc.addKeymapLayer(function(layer_set)
		layer_set({ "n", "x" }, "(", mc.prevCursor)
		layer_set({ "n", "x" }, ")", mc.nextCursor)
		layer_set({ "n", "x" }, "<A-,>", mc.deleteCursor)

		layer_set("n", "<esc>", function()
			if not mc.cursorsEnabled() then
				mc.enableCursors()
			else
				mc.clearCursors()
			end
		end)
	end)

	local hl = vim.api.nvim_set_hl
	hl(0, "MultiCursorCursor", { bg = "#808080" })
	hl(0, "MultiCursorVisual", { link = "Visual" })
	hl(0, "MultiCursorSign", { link = "SignColumn" })
	hl(0, "MultiCursorMatchPreview", { link = "Search" })
	hl(0, "MultiCursorDisabledCursor", { reverse = true })
	hl(0, "MultiCursorDisabledVisual", { link = "Visual" })
	hl(0, "MultiCursorDisabledSign", { link = "SignColumn" })
end)

MiniDeps.now(function()
	MiniDeps.add "prichrd/netrw.nvim"
	require("netrw").setup {} -- pretty netrw with icons
	require("mini.icons").setup {
		style = "glyph",
	}
	require("mini.statusline").setup {
		content = {
			active = function()
				local mode, mode_hl = MiniStatusline.section_mode { trunc_width = 120 }
				local git = MiniStatusline.section_git { trunc_width = 40 }
				local diff = MiniStatusline.section_diff { trunc_width = 75 }
				local diagnostics = MiniStatusline.section_diagnostics { trunc_width = 75 }
				local lsp = MiniStatusline.section_lsp { trunc_width = 75 }
				local filename = MiniStatusline.section_filename { trunc_width = 140 }
				local fileinfo = MiniStatusline.section_fileinfo { trunc_width = 120 }
				local location = MiniStatusline.section_location { trunc_width = 75 }
				local search = MiniStatusline.section_searchcount { trunc_width = 75 }
				local yaml_schema = (function(args)
					if MiniStatusline.is_truncated(args.trunc_width) then
						return ""
					end
					---@type SchemerYamlSchema
					local schema = vim.b.schemer_yaml_schema
					return schema and (schema.name or schema.uri) or ""
				end) { trunc_width = 120 }

				local multicursor = (function(args)
					if MiniStatusline.is_truncated(args.trunc_width) then
						return ""
					end

					local total = 1
					local current = 1

					local ok, mc = pcall(require, "multicursor-nvim")
					local _mode = vim.fn.mode()

					if not ok or not vim.list_contains({ "n", "v", "V" }, _mode) then
						return ""
					end

					mc.action(function(ctx)
						local cursors = ctx:getCursors()
						total = #cursors
						for index, cursor in ipairs(cursors) do
							if cursor:isMainCursor() then
								current = index
								break
							end
						end
					end)

					return total ~= 1 and ("(%d/%d)"):format(current, total) or ""
				end) { trunc_width = 120 }

				return MiniStatusline.combine_groups {
					{ hl = mode_hl, strings = { mode } },
					{ hl = "MiniStatuslineDevinfo", strings = { git, diff, diagnostics, lsp } },
					"%<", -- Mark general truncate point
					{ hl = "MiniStatuslineFilename", strings = { filename } },
					"%=", -- End left alignment
					{ hl = "MiniStatuslineFileinfo", strings = { yaml_schema, fileinfo } },
					{ hl = mode_hl, strings = { multicursor, search, location } },
				}
			end,
			inactive = nil,
		},

		use_icons = true,
		set_vim_settings = true,
	}
	require("mini.tabline").setup {}
end)
