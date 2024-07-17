local settings = require("settings")

local function on_attach(_, bufnr)
	local telescope = require("telescope.builtin")
	local map = function(mode, keys, func, desc)
		vim.keymap.set(mode, keys, func, { buffer = bufnr, desc = desc })
	end

	map("n", "<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
	map("n", "<leader>la", vim.lsp.buf.code_action, "[L]sp Code [A]ction")
	map("n", "<leader>lf", vim.cmd.Format, "[L]SP [F]ormat")
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

local function setup()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	local cmp_installed, cmp_lsp = pcall(require, "cmp_nvim_lsp")
	if cmp_installed then
		capabilities = vim.tbl_deep_extend("force", capabilities, cmp_lsp.default_capabilities())
	end

	require("mason-lspconfig").setup({
		ensure_installed = vim.tbl_keys(settings.language_servers),
		handlers = {
			function(server_name)
				local server = settings.language_servers[server_name] or {}
				server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
				server.on_attach = on_attach
				require("lspconfig")[server_name].setup(server)
			end,
		},
	})
end

return {
	{
		"neovim/nvim-lspconfig",
		config = setup,
	},
	-- Tools manager(lsp, linter, formatter)
	{ "williamboman/mason.nvim", opts = {} },
	"williamboman/mason-lspconfig.nvim",
	-- Progress thingy
	{ "j-hui/fidget.nvim", opts = {} },
	-- Fmt
	{
		"stevearc/conform.nvim",
		config = function()
			local conform = require("conform")

			conform.setup({ formatters_by_ft = settings.formatters })
			local function fmt(_)
				conform.format({ lsp_fallback = true, timeout_ms = 500 })
			end
			vim.api.nvim_create_user_command("Format", fmt, { desc = "Format current buffer with LSP" })
		end,
	},
	-- Lint
	{
		"mfussenegger/nvim-lint",
		config = function()
			require("lint").linters_by_ft = settings.linters

			vim.api.nvim_create_autocmd({ "BufWritePost" }, {
				callback = function()
					require("lint").try_lint()
				end,
			})
		end,
	},
	-- lazydev config
	{
		"folke/lazydev.nvim",
		ft = "lua", -- only load on lua files
		opts = {
			library = {
				-- See the configuration section for more details
				-- Load luvit types when the `vim.uv` word is found
				{ path = "luvit-meta/library", words = { "vim%.uv" } },
			},
		},
		enabled = false,
	},
	{ "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
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
