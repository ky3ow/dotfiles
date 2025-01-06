local H = {}

function H.lsp_attach(_, buf)
	local telescope = require "telescope.builtin"
	local map = function(mode, keys, func, desc)
		vim.keymap.set(mode, keys, func, { buffer = buf, desc = desc })
	end

	local formatprg = vim.fn.exists(":Format") == 2 and vim.cmd.Format or vim.lsp.buf.format -- exists matches fully(==2)

	map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
	map("n", "<leader>la", vim.lsp.buf.code_action, "[L]sp Code [A]ction")
	map("n", "<leader>lf", formatprg, "[L]SP [F]ormat")
	map("n", "<leader>ls", telescope.lsp_document_symbols, "[L]SP document [S]ymbols")

	map("n", "gd", telescope.lsp_definitions, "[G]oto [D]efinition")
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

local add = require("mini.deps").add
local now, later = require("mini.deps").now, require("mini.deps").later

add {
	source = "neovim/nvim-lspconfig",
	depends = {
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"j-hui/fidget.nvim",
		"stevearc/conform.nvim",
		"mfussenegger/nvim-lint",
	}
}

now(function()
	local blink_installed, blink = pcall(require, "blink.cmp")

	require("mason").setup {}
	require("mason-lspconfig").setup {
		handlers = {
			function(server_name)
				local config = vim.g.settings.language_servers[server_name] or {}
				if blink_installed then
					config.capabilities = blink.get_lsp_capabilities(config.capabilities, true)
				else
					config.capabilities = vim.tbl_deep_extend("force", {}, vim.lsp.protocol.make_client_capabilities(), config.capabilities or {})
				end

				config.on_attach = H.lsp_attach
				require("lspconfig")[server_name].setup(config)
			end
		}
	}
end)

later(function()
	require("fidget").setup {}

	local conform = require "conform"
	conform.setup { formatters_by_ft = vim.g.settings.formatters }
	local function fmt(_)
		conform.format { lsp_fallback = true, timeout_ms = 500 }
	end
	vim.api.nvim_create_user_command("Format", fmt, { desc = "Format current buffer with LSP" })

	require("lint").linters_by_ft = vim.g.settings.linters
	vim.api.nvim_create_autocmd({ "BufWritePost" }, {
		callback = function()
			require("lint").try_lint(nil, { ignore_errors = true })
		end,
	})
end)

add {
	source = "folke/lazydev.nvim",
	depends = { "Bilal2453/luvit-meta" }
}

return {
	{
		"hrsh7th/nvim-cmp",
		opts = function(_, opts)
			opts.sources = opts.sources or {}
			table.insert(opts.sources, {
				name = "lazydev",
				group_index = 0, -- set group index to 0 to skip loading LuaLS completions
			})
		end,
	},
}
