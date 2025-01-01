local settings = require "settings"

local spec = {
	{
		"neanias/everforest-nvim",
		theme_name = "everforest",
		config = function()
			require("everforest").setup {
				background = "hard",
				disable_italic_comments = true,
			}
		end,
	},

	{
		"rose-pine/neovim",
		theme_name = {
			"rose-pine",
			"rose-pine-dawn",
			"rose-pine-main",
			"rose-pine-moon",
		},
		config = function()
			require("rose-pine").setup {
				styles = {
					italic = false,
				},
			}
		end,
	},

	{
		"ellisonleao/gruvbox.nvim",
		theme_name = "gruvbox",
		config = function()
			require("gruvbox").setup {
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
			}
		end,
	},
	{
		"EdenEast/nightfox.nvim",
		theme_name = {
			"nightfox",
			"duskfox",
			"carbonfox",
			"nordfox",
			"dawnfox",
			"terafox",
		},
	},
}

---@param target string
---@param source string | table
local function contains(target, source)
	if type(source) == "table" then
		for _, value in ipairs(source) do
			if value == target then
				return true
			end
		end
	end

	if type(source) == "string" then
		return target == source
	end

	return false
end

for _, colorscheme in ipairs(spec) do
	local cfg = colorscheme.config or function() end
	colorscheme.priority = contains(settings.colorscheme, colorscheme.theme_name) and 1000 or 100
	colorscheme.config = function()
		cfg()
		if colorscheme.priority == 1000 then
			vim.cmd.colorscheme(settings.colorscheme)
		end
	end
end

-- [[ Highlight on yank ]]
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = "*",
})

return spec
