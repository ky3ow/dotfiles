local H = {}

H.colorschemes = {
	{
		source = "neanias/everforest-nvim",
		name = "everforest",
		configured = false,
	},

	{
		source = "rose-pine/neovim",
		name = "rose-pine",
		aliases = { "rose-pine-dawn", "rose-pine-main", "rose-pine-moon", },
	},

	{
		source = "ellisonleao/gruvbox.nvim",
		name = "gruvbox",
		configured = false,
	},

	{
		source = "EdenEast/nightfox.nvim",
		name = "nightfox",
		aliases = { "duskfox", "carbonfox", "nordfox", "dawnfox", "terafox", },
		configured = false,
	},
}

---@param target string
---@param source table
function H.contains(target, source)
	for _, value in ipairs(source) do
		if value == target then
			return true
		end
	end

	return false
end

local settings = vim.g.settings
settings.colorschemes = {
	everforest = {
		background = "hard",
		disable_italic_comments = true,
	},

	gruvbox = {
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

	["rose-pine"] = {
		styles = {
			italic = false,
		},
	},

	nightfox = {},
}
vim.g.settings = settings

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
	callback = function(e)
		---@type string
		local name = e.match
		local target_colorscheme

		for _, colorscheme in ipairs(H.colorschemes) do
			if (name == colorscheme.name) or (colorscheme.aliases and H.contains(name, colorscheme.aliases)) then
				target_colorscheme = colorscheme
			end
		end

		if (not target_colorscheme) or target_colorscheme.configured then
			-- colorscheme is not custom or already configured
			return
		end

		local module = require(target_colorscheme.name)

		if not module.setup then
			-- colorscheme is not configured via typical `setup`
			return
		end

		module.setup(vim.g.settings.colorschemes[target_colorscheme.name] or {})
		target_colorscheme.configured = true
	end,
})

local add = require("mini.deps").add
local now = require("mini.deps").now

for _, colorscheme in ipairs(H.colorschemes) do
	add {
		source = colorscheme.source
	}
end

now(function()
	vim.cmd.colorscheme(vim.g.settings.colorscheme)
end)
