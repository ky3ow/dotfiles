MiniDeps.now(function()
	MiniDeps.add "neovim/nvim-lspconfig"
	MiniDeps.add "williamboman/mason.nvim"
	MiniDeps.add "folke/lazydev.nvim"
	MiniDeps.add "Bilal2453/luvit-meta"

	local blink_installed, blink = pcall(require, "blink.cmp")

	require("mason").setup {}
	for server, config in pairs(vim.g.language_servers) do
		if blink_installed then
			config.capabilities = blink.get_lsp_capabilities(config.capabilities, true)
		else
			config.capabilities = vim.tbl_deep_extend(
				"force",
				{},
				vim.lsp.protocol.make_client_capabilities(),
				config.capabilities or {}
			)
		end

		require("lspconfig")[server].setup(config)
	end

	vim.api.nvim_create_user_command("Format", function(_)
		vim.notify("Formatting with lsp...")
		vim.lsp.buf.format()
	end, { desc = "Format current buffer with LSP" })

	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("ky3ow.LspAttach", { clear = true }),
		callback = function(e)
			local map = function(mode, keys, func, desc)
				vim.keymap.set(mode, keys, func, { buffer = e.buf, desc = desc })
			end
			map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
			map("n", "<leader>la", vim.lsp.buf.code_action, "[L]SP [A]ction")
			map("n", "<leader>lf", "<cmd>Format<cr>", "[L]SP [F]ormat")
			map("n", "<leader>ls", "<cmd>Pick lsp scope='document_symbol'<cr>", "[L]SP document [S]ymbols")

			map("n", "gd", "<cmd>Pick lsp scope='definition'<cr>", "[G]oto [D]efinition")
			map("n", "gr", "<cmd>Pick lsp scope='references'<cr>", "[G]oto [R]eferences")
			map("n", "gI", "<cmd>Pick lsp scope='implementation'<cr>", "[G]oto [I]mplementation")

			map("n", "K", vim.lsp.buf.hover, "Hover Documentation")
			map({ "n", "i" }, "<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

			map("n", "gD", "<cmd>Pick lsp scope='declaration'<cr>", "[G]oto [D]eclaration")
			map("n", "<leader>ws", "<cmd>Pick lsp scope='workspace_symbol'<cr>", "[W]orkspace [S]ymbols")
			map("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
			map("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
			map("n", "<leader>wl", function()
				print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
			end, "[W]orkspace [L]ist Folders")
		end
	})
end)

MiniDeps.later(function()
	MiniDeps.add "stevearc/conform.nvim"
	MiniDeps.add "mfussenegger/nvim-lint"

	require("mini.notify").setup {
		window = {
			config = function()
				return { anchor = 'SE', col = vim.o.columns, row = vim.o.lines - vim.o.cmdheight - 1, border = "none" }
			end
		}
	}
	vim.notify = MiniNotify.make_notify {
		--DEBUG = { duration = 100, hl_group = 'DiagnosticHint' },
	}

	local notify_commands = { "clear", "history", "refresh" }
	vim.api.nvim_create_user_command("Notify", function(opts)
		local action = opts.args
		if action == "clear" then
			MiniNotify.clear()
		elseif action == "history" then
			MiniNotify.show_history()
		elseif action == "refresh" then
			MiniNotify.refresh()
		end
	end, {
		nargs = 1,
		complete = function(arg_lead, cmd_line, cursor_pos)
			local matches = require("mini.fuzzy").filtersort(arg_lead, notify_commands)
			if #matches == 0 then
				return notify_commands
			end
			return matches
		end
	})

	local conform = require "conform"
	conform.setup { formatters_by_ft = vim.g.formatters }

	vim.api.nvim_create_user_command("Format", function(_)
		vim.notify("Formatting with conform...")
		conform.format { lsp_fallback = true, timeout_ms = 500 }
	end, { desc = "Format current buffer with LSP" })

	require("lint").linters_by_ft = vim.g.linters
	for linter, config in pairs(vim.g.linter_configs) do
		local linters = require "lint".linters
		linters[linter] = vim.tbl_deep_extend("force", linters[linter] --[[@as lint.Linter]] or {}, config)
	end
	vim.api.nvim_create_autocmd({ "BufWritePost" }, {
		callback = function()
			require("lint").try_lint(nil, { ignore_errors = true })
		end,
	})
	vim.api.nvim_create_user_command("Lint", function(_)
		vim.notify("Linting...")
		require("lint").try_lint(nil)
	end, { desc = "Run linter" })

	vim.keymap.set("n", "<leader>lf", "<cmd>Format<cr>", { desc = "[L]anguage [F]ormat" })
end)
