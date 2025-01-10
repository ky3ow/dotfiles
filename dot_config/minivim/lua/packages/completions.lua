local add = require("mini.deps").add
local later = require("mini.deps").later

later(function()
	add {
		source = "saghen/blink.cmp",
		depends = { "rafamadriz/friendly-snippets" },
		checkout = "v0.10.0",
	}
	require("mini.icons").setup {}
	-- @TODO look into MiniSnippets.default_insert() to add dynamic node logic
	-- use when it could accept lsp completions
	-- local gen_loader = require("mini.snippets").gen_loader
	-- require("mini.snippets").setup {
	-- 	snippets = {
	-- 		gen_loader.from_lang(),
	-- 		gen_loader.from_runtime("globals.json"),
	-- 		gen_loader.from_runtime("globals.lua"),
	-- 	},
	-- }
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
			default = { "lazydev", "lsp", "path", "snippets", "buffer" },
			cmdline = {},
			providers = {
				lazydev = {
					name = "LazyDev",
					module = "lazydev.integrations.blink",
					score_offset = 100,
				},
			},
		},

		completion = {
			documentation = {
				auto_show = true,
				auto_show_delay_ms = 200,
			},

			list = {
				selection = {
					preselect = true,
					auto_insert = true,
				},
			},

			menu = {
				draw = {
					columns = {
						{ "label", "label_description", gap = 1 },
						{ "kind",  "source_name",       gap = 1 },
					},
					gap = 2,
					components = {
						kind = {
							ellipsis = false,
							text = function(ctx)
								return "(" .. ctx.kind .. ")"
							end
						},
						source_name = {
							width = { max = 30 },
							text = function(ctx)
								local name = ctx.source_name
								local map = {
									Snippets = "[snip]",
									LSP = "[lsp]",
									Path = "[path]",
									Buffer = "[buf]"
								}
								return map[name]
							end,
							highlight = 'BlinkCmpSource',
						}
					},
				},
			},
		},

		snippets = { preset = "default" },
	}
end)
