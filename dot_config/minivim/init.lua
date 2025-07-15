-- [[ Leader ]]
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.g.colors_name = "ef-elea-dark"

vim.g.formatters = {
	lua = { "stylua" },
	python = { "black" },
	-- Sequential
	-- python = { "isort", "black" },
	-- First found
	-- javascript = { { "prettierd", "prettier" } },
}

vim.g.linters = {
	python = { "flake8" },
	yaml = { "yamllint" },
}

vim.g.linter_configs = {
	yamllint = {
		args = {
			"--format",
			"parsable",
			"-d",
			"{extends: relaxed, rules: {indentation: {indent-sequences: whatever}}}",
			"-",
		},
	},
}

-- vim.lsp.set_log_level("DEBUG")
vim.g.language_servers = {
	matlab_ls = {
		filetypes = { "matlab" },
		settings = {
			matlab = {
				installPath = vim.fn.expand "~/.local/MATLAB/",
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
	yamlls = {
		settings = {
			yaml = {
				completion = true,
				validate = true,
				hover = true,
				schemas = {},
				schemaStore = {
					enable = true,
					url = "https://www.schemastore.org/api/json/catalog.json",
				},
				schemaDownload = {
					enable = true,
				},
				-- trace = {
				-- 	server = "debug",
				-- },
			},
		},
	},
	powershell_es = {
		bundle_path = vim.fn.stdpath "data" .. "/mason/packages/powershell-editor-services",
		capabilities = vim.lsp.protocol.make_client_capabilities(),
	},
}

local path_package = vim.fn.stdpath "data" .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not (vim.uv or vim.loop).fs_stat(mini_path) then
	vim.cmd 'echo "Installing `mini.nvim`" | redraw'
	local clone_cmd = {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/echasnovski/mini.nvim",
		mini_path,
	}
	vim.fn.system(clone_cmd)
	vim.cmd "packadd mini.nvim | helptags ALL"
	vim.cmd 'echo "Installed `mini.nvim`" | redraw'
end

local path_binary = vim.fn.expand(vim.fn.stdpath "data" .. "/bin")
local path_separator = vim.uv.os_uname().sysname == "Windows_NT" and ";" or ":"
vim.env.PATH = path_binary .. path_separator .. vim.env.PATH
vim.g.path_binary = path_binary

vim.g.mini_deps = path_package .. "pack/deps/"
require("mini.deps").setup { path = { package = path_package } }

--Source all packages
vim.cmd.runtime { "lua/packages/*.lua", bang = true }
