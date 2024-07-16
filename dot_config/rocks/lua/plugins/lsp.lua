-- Config language servers
local servers = {
	-- clangd = {},
	-- gopls = {},
	-- pyright = {},
	-- rust_analyzer = {},
	-- tsserver = {},
	-- html = { filetypes = { 'html', 'twig', 'hbs'} },
	lua_ls = {
		settings = {
			Lua = {
				workspace = { checkThirdParty = false },
				telemetry = { enable = false },
				diagnostics = {
					disable = { "missing-fields" },
					globals = { "vim" },
				},
			},
		},
	},
}

-- By filetype
local formatters = {
	lua = { "stylua" },
	python = { "black" },
	-- Conform can also run multiple formatters sequentially
	-- python = { "isort", "black" },
	-- You can use a sub-list to tell conform to run *until* a formatter
	-- is found.
	-- javascript = { { "prettierd", "prettier" } },
}

local linters = {
	python = { "flake8" },
}

local function on_attach(_, bufnr)
	local telescope = require("telescope.builtin")
	local map = function(mode, keys, func, desc)
		vim.keymap.set(mode, keys, func, { buffer = bufnr, desc = desc })
	end

	local function fmt(_)
		require("conform").format({ lsp_fallback = true, timeout_ms = 500 })
	end

	map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
	map("n", "<leader>la", vim.lsp.buf.code_action, "[L]sp Code [A]ction")
	map("n", "<leader>lf", fmt, "[L]SP [F]ormat")
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

	vim.api.nvim_buf_create_user_command(bufnr, "Format", fmt, { desc = "Format current buffer with LSP" })
end

-- Spinny thingy
require("fidget").setup {}

-- Lsp
local capabilities = vim.lsp.protocol.make_client_capabilities()
local cmp_installed, cmp_lsp = pcall(require, "cmp_nvim_lsp")
if cmp_installed then
	capabilities = vim.tbl_deep_extend("force", capabilities, cmp_lsp.default_capabilities())
end

require("mason").setup {}
require("mason-lspconfig").setup {
	ensure_installed = vim.tbl_keys(servers),
	handlers = {
		function(server_name)
			local server = servers[server_name] or {}
			server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
			server.on_attach = on_attach
			require("lspconfig")[server_name].setup(server)
		end,
	},
}

-- Formatter
require("conform").setup {
	formatters_by_ft = formatters,
}

-- Linter
local linter = require("lint")
linter.linters_by_ft = linters

local function lint()
	linter.try_lint()
end

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	callback = lint
})
vim.api.nvim_create_user_command("Lint", lint, { desc = "Lint current buffer" })
