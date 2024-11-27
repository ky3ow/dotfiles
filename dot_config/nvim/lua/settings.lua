return {
	colorscheme = "everforest",

	language_servers = {
		-- clangd = {},
		-- gopls = {},
		matlab_ls = {
			filetypes = { "matlab" },
			settings = {
				matlab = {
					installPath = "/home/v/.local/MATLAB/"
				},
			},
			single_file_support = true
		},
		pyright = {},
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
	},

	-- By filetype
	formatters = {
		lua = { "stylua" },
		python = { "black" },
		-- Conform can also run multiple formatters sequentially
		-- python = { "isort", "black" },
		-- You can use a sub-list to tell conform to run *until* a formatter
		-- is found.
		-- javascript = { { "prettierd", "prettier" } },
	},

	linters = {
		python = { "flake8" },
	},
}
