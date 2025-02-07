---@class Colorscheme
---@field names table<string>
---@field setup? function
---@field configured? boolean

---@type table<string, Colorscheme>
vim.g.colorschemes = {
	["neanias/everforest-nvim"] = {
		names = { "everforest" },
		setup = function()
			require "everforest".setup {
				background = "hard",
				disable_italic_comments = true,
				on_highlights = function(hl, pallette)
					hl.NormalFloat = { bg = pallette.bg0 }
					hl.MiniPickMatchCurrent = { bg = pallette.bg_green }
				end,
			}
		end,
	},

	["ellisonleao/gruvbox.nvim"] = {
		names = { "gruvbox" },
		setup = function()
			require "gruvbox".setup {
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

	["rose-pine/neovim"] = {
		names = { "rose-pine", "rose-pine-dawn", "rose-pine-main", "rose-pine-moon", },
		setup = function()
			require "rose-pine".setup {
				styles = {
					italic = false,
				},
			}
		end,
	},

	["EdenEast/nightfox.nvim"] = {
		names = { "nightfox", "duskfox", "carbonfox", "nordfox", "dawnfox", "terafox", },
		setup = function()
			require "nightfox".setup {}
		end
	},

	["oonamo/ef-themes.nvim"] = {
		names = {
			"ef-autumn", "ef-bio", "ef-cherie", "ef-dark",
			"ef-deuteranopia-dark", "ef-dream", "ef-duo-dark",
			"ef-elea-dark", "ef-maris-dark", "ef-melissa-dark",
			"ef-night", "ef-owl", "ef-rosa", "ef-symbiosis",
			"ef-trio-dark", "ef-tritanopia-dark", "ef-winter",
		},
		setup = function()
			require "ef-themes".setup {
				dark = "ef-elea-dark",
				on_colors = function(colors, name)
					if name == "ef-elea-dark" then
						colors.fg_main = "#ebf3f0"
						colors.fg_mode_line = "#bed7cd"
						colors.bg_alt = "#383a3a"
					end
				end,
				styles = {
					comments = { italic = false },
				},
			}
		end
	}
}

-- [[ Highlight on yank ]]
vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	pattern = "*",
	callback = function()
		vim.highlight.on_yank()
	end,
})

---@param colorschemes table<string, Colorscheme>
---@param target string
local function get_colorscheme_settings(colorschemes, target)
	for _, settings in pairs(colorschemes) do
		for _, name in ipairs(settings.names) do
			if target == name then
				return settings
			end
		end
	end
end

vim.api.nvim_create_autocmd("ColorSchemePre", {
	group = vim.api.nvim_create_augroup("ky3ow.ColorschemeSetup", { clear = true }),
	callback = function(event)
		---@type string
		local target = event.match
		local colorscheme = get_colorscheme_settings(vim.g.colorschemes, target)

		if (not colorscheme) or colorscheme.configured then
			-- colorscheme is not custom or already configured
			return
		end

		vim.validate {
			setup = { colorscheme.setup, "function", false },
			names = { colorscheme.names, "table", false },
		}

		if colorscheme.setup then
			colorscheme.setup()
		end

		colorscheme.configured = true
	end,
})

MiniDeps.now(function()
	for source, _ in pairs(vim.g.colorschemes) do
		MiniDeps.add(source)
	end
	vim.cmd.colorscheme(vim.g.colors_name)
end)
