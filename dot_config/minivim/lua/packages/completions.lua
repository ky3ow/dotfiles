local add = require("mini.deps").add
local later = require("mini.deps").later

add {
	source = "saghen/blink.cmp",
	depends = { "rafamadriz/friendly-snippets", "L3MON4D3/LuaSnip" },
	checkout = "v0.9.3",
}

later(function()
	require("blink.cmp").setup {
		keymap = {
			preset = "default",
			["<C-l>"] = { "snippet_forward", "fallback" },
			["<C-h>"] = { "snippet_backward", "fallback" },
		},
		appearance = {
			use_nvim_cmp_as_default = true,
			nerd_font_variant = "mono",
		},

		sources = {
			default = { "lsp", "path", "luasnip", "buffer" },
			cmdline = {},
		},

		completion = {
			documentation = {
				auto_show = true,
				auto_show_delay_ms = 200,
			},
			list = {
				selection = "auto_insert"
			},
		},

		snippets = {
			expand = function (snippet)
				require("luasnip").lsp_expand(snippet)
			end,
			active = function (filter)
				if filter and filter.direction then
					return require("luasnip").jumpable(filter.direction)
				end
				return require("luasnip").in_snippet()
			end,
			jump = function (direction)
				require("luasnip").jump(direction)
			end
		},
	}

	require("luasnip.loaders.from_vscode").lazy_load()
	-- local ls = require "luasnip"
	-- ls.filetype_extend("javascript", { "jsdoc" })
end)
