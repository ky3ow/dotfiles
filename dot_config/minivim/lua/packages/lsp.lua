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
			local telescope = require "telescope.builtin"
			local map = function(mode, keys, func, desc)
				vim.keymap.set(mode, keys, func, { buffer = e.buf, desc = desc })
			end

			map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
			map("n", "<leader>la", vim.lsp.buf.code_action, "[L]sp Code [A]ction")
			map("n", "<leader>lf", vim.cmd.Format, "[L]SP [F]ormat")
			map("n", "<leader>ls", telescope.lsp_document_symbols, "[L]SP document [S]ymbols")

			map("n", "gd", vim.lsp.buf.definition, "[G]oto [D]efinition")

			local function on_list(opts)
				local function jump_to(item)
					if not item then
						return
					end
					vim.lsp.util.jump_to_location(item.user_data, "utf-8")
				end

				local locations = {}
				local items = {}
				for idx, item in ipairs(opts.items) do
					locations[idx] = item.user_data
					items[idx] = {
						col = item.col,
						lnum = item.lnum,
						user_data = item.user_data,
						filename = item.filename,
						path = item.filename,
						text = item.text,
					}
				end

				if #opts.items > 1 then
					vim.fn.setqflist(vim.lsp.util.locations_to_items(locations, "utf-8"))
					vim.ui.select(items, {
						prompt = "Lsp Locations",
						format_item = function(item)
							local filename = vim.fn.pathshorten(vim.fn.fnamemodify(item.filename, ":p:."), 3)
							return string.format("%s:%d:%d:%s", filename, item.lnum, item.col, item.text)
						end,
						-- preview_item = function(item)
						-- 	-- check Minipick.default_preview
						-- 	return {
						-- 		path = item.filename,
						-- 		col = item.col,
						-- 		lnum = item.lnum,
						-- 		text = item.text
						-- 	}
						-- end,
						-- preview = MiniPick.default_preview
					}, jump_to)
				else
					jump_to(items[1])
				end
			end
			require("mini.pick").setup {}
			vim.ui.select = MiniPick.ui_select

			map("n", "gd", function() vim.lsp.buf.definition({ on_list = on_list }) end, "[G]oto [D]efinition")
			map("n", "gr", telescope.lsp_references, "[G]oto [R]eferences")
			map("n", "gI", telescope.lsp_implementations, "[G]oto [I]mplementation")
			map("n", "<leader>D", telescope.lsp_type_definitions, "Type [D]efinition")

			map("n", "K", vim.lsp.buf.hover, "Hover Documentation")
			map({ "n", "i" }, "<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

			map("n", "gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
			map("n", "<leader>ws", telescope.lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
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

	require("fidget").setup {}

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
end)
