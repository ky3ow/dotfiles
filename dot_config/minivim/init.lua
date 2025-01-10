-- [[ Leader ]]
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.g.settings = {
	colorscheme = "everforest",

	language_servers = {
		matlab_ls = {
			filetypes = { "matlab" },
			settings = {
				matlab = {
					installPath = "/home/v/.local/MATLAB/",
				},
			},
			single_file_support = true,
		},
		pyright = {},
		terraformls = {},
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
		-- Sequential
		-- python = { "isort", "black" },
		-- First found
		-- javascript = { { "prettierd", "prettier" } },
	},

	linters = {
		python = { "flake8" },
	},
}

-- Bootstrap package manager
vim.cmd.runtime { "lua/bootstrap.lua" }
-- Source all non-package stuff 
vim.cmd.runtime { "lua/scripts/*.lua", bang = true }
-- Source all package-stuff
vim.cmd.runtime { "lua/packages/*.lua", bang = true }
