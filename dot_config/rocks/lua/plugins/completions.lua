require("luasnip.loaders.from_vscode").lazy_load()
-- ls.filetype_extend("javascript", { "jsdoc" })
local cmp = require("cmp")
local ls = require("luasnip")

cmp.setup {
	snippet = {
		expand = function(args)
			ls.lsp_expand(args.body)
		end,
	},
	completion = { completeopt = "menu,menuone,noinsert" },
	mapping = cmp.mapping.preset.insert({
		["<C-n>"] = cmp.mapping.select_next_item(),
		["<C-p>"] = cmp.mapping.select_prev_item(),
		["<C-y>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-l>"] = cmp.mapping(function()
			if ls.expand_or_locally_jumpable() then
				ls.expand_or_jump()
			end
		end, { "i", "s" }),
		["<C-h>"] = cmp.mapping(function()
			if ls.locally_jumpable(-1) then
				ls.jump(-1)
			end
		end, { "i", "s" }),
		["<C-j>"] = cmp.mapping(function()
			if ls.choice_active() then
				ls.change_choice(1)
			end
		end, { "i", "s" }),
	}),
	formatting = {
		format = function(entry, vim_item)
			vim_item.kind = "(" .. string.lower(vim_item.kind) .. ")"
			vim_item.menu = ({
				nvim_lsp = "[lsp]",
				luasnip = "[snip]",
				buffer = "[bufr]",
				path = "[path]",
			})[entry.source.name]
			return vim_item
		end,
	},
	sources = cmp.config.sources({
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
		{ name = "buffer" },
		{ name = "path" },
	}),
}
