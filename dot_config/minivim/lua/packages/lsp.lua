local add = require("mini.deps").add
local now, later = require("mini.deps").now, require("mini.deps").later

now(function()
	add "neovim/nvim-lspconfig"
	add "williamboman/mason.nvim"
	add "folke/lazydev.nvim"
	add "Bilal2453/luvit-meta"

	local blink_installed, blink = pcall(require, "blink.cmp")

	require("mason").setup {}
	for server, config in pairs(vim.g.settings.language_servers) do
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
		vim.notify("Formatting with lsp")
		vim.lsp.buf.format()
	end, { desc = "Format current buffer with LSP" })

	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("ky3ow.LspAttach", { clear = true }),
		callback = function(e)
			local map = function(mode, keys, func, desc)
				vim.keymap.set(mode, keys, func, { buffer = e.buf, desc = desc })
			end

			map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
			map("n", "<leader>la", vim.lsp.buf.code_action, "[L]sp Code [A]ction")
			map("n", "<leader>lf", "<cmd>Format<cr>", "[L]SP [F]ormat")
			map("n", "<leader>ls", "<cmd>Pick lsp scope='document_symbol'<cr>", "[L]SP document [S]ymbols")

			map("n", "gd", "<cmd>Pick lsp scope='definition'<cr>", "[G]oto [D]efinition")
			map("n", "gr", "<cmd>Pick lsp scope='references'<cr>", "[G]oto [R]eferences")
			map("n", "gI", "<cmd>Pick lsp scope='implementation'<cr>", "[G]oto [I]mplementation")
			map("n", "<leader>D", "<cmd>Pick lsp scope='type_definitions'<cr>", "Type [D]efinition")

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

later(function()
	add "j-hui/fidget.nvim"
	add "stevearc/conform.nvim"
	add "mfussenegger/nvim-lint"

	require("mini.notify").setup {
		window = {
			config = function()
				return { anchor = 'SE', col = vim.o.columns, row = vim.o.lines - vim.o.cmdheight - 1, border = "none" }
			end
		}
	}
	vim.notify = MiniNotify.make_notify()
	-- require("fidget").setup {}
	-- vim.notify = require("fidget.notification").notify

	local conform = require "conform"
	conform.setup { formatters_by_ft = vim.g.settings.formatters }

	vim.api.nvim_create_user_command("Format", function(_)
		vim.notify("Formatting with conform")
		conform.format { lsp_fallback = true, timeout_ms = 500 }
	end, { desc = "Format current buffer with LSP" })

	require("lint").linters_by_ft = vim.g.settings.linters
	vim.api.nvim_create_autocmd({ "BufWritePost" }, {
		callback = function()
			require("lint").try_lint(nil, { ignore_errors = true })
		end,
	})

	vim.keymap.set("n", "<leader>lf", "<cmd>Format<cr>", { desc = "[L]anguage [F]ormat" })
end)
