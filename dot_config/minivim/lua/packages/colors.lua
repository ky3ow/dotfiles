local g_settings = vim.g.settings
---@class Colorscheme
---@field source string
---@field config? table
---@field pre_init? function
---@field aliases? table<string>
---@field configured? boolean

---@type table<string, Colorscheme>
g_settings.colorschemes = {
	everforest = {
		source = "neanias/everforest-nvim",
		config = {
			background = "hard",
			disable_italic_comments = true,
			on_highlights = function (hl, pallette)
				hl.NormalFloat = { bg = pallette.bg0 }
				hl.MiniPickMatchCurrent = { bg = pallette.bg_green }
			end,
		},
	},

	gruvbox = {
		source = "ellisonleao/gruvbox.nvim",
		config = {
			undercurl = false,
			underline = false,
			bold = true,
			italic = {
				strings = false,
				emphasis = true,
				comments = false,
				operators = false,
				folds = false,
			},
		},
	},

	["rose-pine"] = {
		source = "rose-pine/neovim",
		aliases = { "rose-pine-dawn", "rose-pine-main", "rose-pine-moon", },
		config = {
			styles = {
				italic = false,
			},
		},
	},

	nightfox = {
		source = "EdenEast/nightfox.nvim",
		aliases = { "duskfox", "carbonfox", "nordfox", "dawnfox", "terafox", },
	},
}
vim.g.settings = g_settings

---@param target string
---@param source table
local function contains(source, target)
	for _, value in ipairs(source) do
		if value == target then
			return true
		end
	end

	return false
end

-- [[ Highlight on yank ]]
vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	pattern = "*",
	callback = function()
		vim.highlight.on_yank()
	end,
})

vim.api.nvim_create_autocmd("ColorSchemePre", {
	group = vim.api.nvim_create_augroup("ky3ow.ColorschemeSetup", { clear = true }),
	callback = function(event)
		---@type string
		local target = event.match

		local saved_settings = vim.g.settings
		local colorschemes = saved_settings.colorschemes

		local colorscheme = nil

		for name, settings in pairs(colorschemes) do
			if (name == target) or (settings.aliases and contains(settings.aliases, target)) then
				colorscheme = { name = name, settings = settings }
			end
		end

		if (not colorscheme) or colorscheme.settings.configured then
			-- colorscheme is not custom or already configured
			return
		end

		vim.validate {
			source = { colorscheme.settings.source, "string", false },
			config = { colorscheme.settings.config, "table", true },
			pre_init = { colorscheme.settings.pre_init, "function", true },
			aliases = { colorscheme.settings.aliases, "table", true },
		}

		if colorscheme.settings.pre_init then
			colorscheme.settings.pre_init()
		end

		if colorscheme.settings.config then
			require(colorscheme.name).setup(colorscheme.settings.config)
		end

		colorscheme.settings.configured = true
		vim.g.settings = saved_settings
	end,
})

local add = require("mini.deps").add
local now = require("mini.deps").now

for _, colorscheme in pairs(vim.g.settings.colorschemes) do
	add {
		source = colorscheme.source
	}
end

now(function()
	vim.cmd.colorscheme(vim.g.settings.colorscheme)
end)
