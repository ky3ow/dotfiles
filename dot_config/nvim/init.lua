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
	astro = {},
	ts_ls = {},
	tailwindcss = {},
	gopls = {},
	pyright = {},
	terraformls = {
		callback = function(client)
			-- ---@cast client vim.lsp.Client
			-- client.server_capabilities.semanticTokensProvider = nil
		end
	},
	-- html = { filetypes = { 'html', 'twig', 'hbs'} },
	lua_ls = {
		settings = {
			Lua = {
				runtime = { version = "LuaJIT", path = vim.split(package.path, ";")},
				workspace = { checkThirdParty = false, ignoreSubmodules = true },
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
	azure_pipelines_ls = {
		filetypes = { "azurepipelines" },
		settings = {
			yaml = {
				completion = true,
				validate = true,
				hover = true,
				schemaStore = {
					enable = true,
					url = "https://www.schemastore.org/api/json/catalog.json",
				},
				schemaDownload = {
					enable = true,
				},
				schemas = {
					["https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json"] = {
						"*.yaml",
						"*.yml",
					},
				},
			},
		},
	},
	gh_actions_ls = {
		filetypes = { "gh" },
	},
	tinymist = {},
}

_G.Config = {}

vim.pack.add { "https://github.com/nvim-mini/mini.nvim" }

local misc = require "mini.misc"

Config.now = function(f) misc.safely('now', f) end
Config.later = function(f) misc.safely('later', f) end
Config.on_event = function(ev, f) misc.safely('event:' .. ev, f) end
Config.on_filetype = function(ft, f) misc.safely('filetype:' .. ft, f) end

---@alias PackChangeType
---| "install"
---| "update"
---| "delete"

---@class PackChangedEventData
---@field active boolean
---@field kind PackChangeType
---@field spec vim.pack.Spec
---@field path string

---@class PackChangedEvent: vim.api.keyset.create_autocmd.callback_args
---@field data PackChangedEventData

---@param plugin string
---@param kinds PackChangeType[]
---@param callback fun()
Config.on_packchanged = function(plugin, kinds, callback)
	vim.api.nvim_create_autocmd("PackChanged", { callback = function(ev)
		---@cast ev PackChangedEvent
		local name, kind = ev.data.spec.name, ev.data.kind
		if not (name == plugin and vim.tbl_contains(kinds, kind)) then return end

		if not ev.data.active then vim.cmd.packadd(plugin) end
		callback()
	end})
end

local path_binary = vim.fn.expand(vim.fn.stdpath "data" .. "/bin")
local path_separator = vim.uv.os_uname().sysname == "Windows_NT" and ";" or ":"
vim.env.PATH = path_binary .. path_separator .. vim.env.PATH

vim.g.path_package = vim.fn.stdpath "data" .. "/site/pack/core"
vim.g.path_binary = path_binary

--Source all packages
vim.cmd.runtime { "lua/packages/*.lua", bang = true }
