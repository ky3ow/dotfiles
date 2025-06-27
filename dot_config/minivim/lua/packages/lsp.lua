local au = vim.api.nvim_create_autocmd
local command = vim.api.nvim_create_user_command

MiniDeps.now(function()
	MiniDeps.add "folke/lazydev.nvim"
	MiniDeps.add "Bilal2453/luvit-meta"
	MiniDeps.add "rafamadriz/friendly-snippets"
	local completion = require "mini.completion"

	completion.setup {
		delay = {
			completion = 10,
			info = 200,
			signature = 200,
		},
		lsp_completion = {
			source_func = "omnifunc",
			auto_setup = false,
		},
	}
	-- @TODO look into MiniSnippets.default_insert() to add dynamic node logic?(if i ever need that?)
	local gen_loader = require("mini.snippets").gen_loader
	require("mini.snippets").setup {
		snippets = {
			gen_loader.from_lang(),
			gen_loader.from_runtime "globals.json",
			gen_loader.from_runtime "globals.lua",
		},
	}

	vim.lsp.config("*", {
		capabilities = vim.tbl_deep_extend(
			"force",
			{},
			vim.lsp.protocol.make_client_capabilities(),
			completion.get_lsp_capabilities()
		),
	})

	au("LspAttach", {
		group = vim.api.nvim_create_augroup("ky3ow.LspAttach", { clear = true }),
		callback = function(e)
			vim.bo[e.buf].omnifunc = "v:lua.MiniCompletion.completefunc_lsp"

			local map = function(mode, keys, func, desc)
				vim.keymap.set(mode, keys, func, { buffer = e.buf, desc = desc })
			end
			map("n", "grn", vim.lsp.buf.rename, "Lsp Rename")
			map("n", "gra", vim.lsp.buf.code_action, "Code Action")
			map("n", "grs", "<cmd>Pick lsp scope='document_symbol'<cr>", "Lsp Symbols")

			map("n", "gd", "<cmd>Pick lsp scope='definition'<cr>", "Goto Definition")
			map("n", "grr", "<cmd>Pick lsp scope='references'<cr>", "Goto References")
			map("n", "gri", "<cmd>Pick lsp scope='implementation'<cr>", "Goto Implementation")

			map("n", "K", vim.lsp.buf.hover, "Hover Documentation")
			map({ "n", "i" }, "<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

			map("n", "grd", "<cmd>Pick lsp scope='declaration'<cr>", "Goto Declaration")
			map("n", "<leader>ws", "<cmd>Pick lsp scope='workspace_symbol'<cr>", "Workspace symbols")
			map("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, "Workspace Add Folder")
			map("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, "Workspace Remove Folder")
			map("n", "<leader>wl", function()
				vim.print(vim.lsp.buf.list_workspace_folders())
			end, "Workspace List Folders")
		end,
	})

	require("mini.notify").setup {
		window = {
			config = function()
				return { anchor = "SE", col = vim.o.columns, row = vim.o.lines - vim.o.cmdheight - 1, border = "none" }
			end,
		},
	}

	local regular_notify = MiniNotify.make_notify {}
	local debug_notify = MiniNotify.make_notify {
		DEBUG = { duration = 1000, hl_group = 'DiagnosticHint' },
	}

	vim.notify = regular_notify

	local notify_commands = { "clear", "history", "refresh", "toggle_debug" }
	command("Notify", function(opts)
		local action = opts.args
		if action == "clear" then
			MiniNotify.clear()
		elseif action == "history" then
			MiniNotify.show_history()
		elseif action == "refresh" then
			MiniNotify.refresh()
		elseif action == "toggle_debug" then
			if vim.g.notify_debug == true then
				vim.notify = regular_notify
				vim.g.notify_debug = false
			else
				vim.notify = debug_notify
				vim.g.notify_debug = true
			end
		end
	end, {
		nargs = 1,
		complete = function(arg_lead, cmd_line, cursor_pos)
			local matches = require("mini.fuzzy").filtersort(arg_lead, notify_commands)
			if #matches == 0 then
				return notify_commands
			end
			return matches
		end,
	})
end)

MiniDeps.later(function()
	MiniDeps.add "williamboman/mason.nvim"
	MiniDeps.add "neovim/nvim-lspconfig"
	MiniDeps.add "stevearc/conform.nvim"
	MiniDeps.add "mfussenegger/nvim-lint"

	require("mason").setup {}
	for server, config in pairs(vim.g.language_servers) do
		vim.lsp.config(server, config)
		vim.lsp.enable(server)
	end

	vim.schedule(function()
		vim.api.nvim_exec_autocmds("BufRead", { buffer = 0 })
	end)

	local conform = require "conform"
	conform.setup { formatters_by_ft = vim.g.formatters }

	command("Format", function(_)
		vim.notify "Formatting with conform..."
		conform.format { lsp_format = "fallback", timeout_ms = 500 }
	end, { desc = "Format current buffer with LSP", range = true })

	vim.g.conformop = function(type)
		type = type or ""
		if type == "" then
			vim.o.operatorfunc = "v:lua.vim.g.conformop"
			return "g@"
		end
		local row_start, row_end = vim.fn.line "'[", vim.fn.line "']"
		local col_start, col_end = 0, vim.fn.strlen(vim.fn.getline(row_end))

		conform.format {
			lsp_format = "fallback",
			timeout_ms = 500,
			range = {
				start = { row_start, col_start },
				["end"] = { row_end, col_end },
			},
		}
	end

	local lint = require "lint"
	lint.linters_by_ft = vim.g.linters
	for linter, config in pairs(vim.g.linter_configs) do
		local linters = lint.linters
		linters[linter] = vim.tbl_deep_extend("force", linters[linter] --[[@as lint.Linter]] or {}, config)
	end
	au({ "BufWritePost" }, {
		callback = function()
			lint.try_lint(nil, { ignore_errors = true })
		end,
	})
	command("Lint", function(_)
		vim.notify "Linting..."
		lint.try_lint(nil)
	end, { desc = "Run linter" })

	vim.keymap.set({ "n", "x" }, "gq", vim.g.conformop, { desc = "Format", expr = true })
	vim.keymap.set("n", "gqq", "<cmd>Format<cr>", { desc = "Format" })
	vim.keymap.set({ "n", "x" }, "gl", "<cmd>Lint<cr>", { desc = "Lint" })
end)
