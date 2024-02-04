local function setup_cmp()
	local cmp = require("cmp")
	local ls = require("luasnip")
	cmp.setup({
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
	})
end

local function setup_snippets()
	require("luasnip.loaders.from_vscode").lazy_load()
	local ls = require("luasnip")
	-- ls.filetype_extend("javascript", { "jsdoc" })
	vim.keymap.set({ "i", "s" }, "<C-L>", function()
		ls.jump(1)
	end, { silent = true })
	vim.keymap.set({ "i", "s" }, "<C-H>", function()
		ls.jump(-1)
	end, { silent = true })
	vim.keymap.set({ "i", "s" }, "<C-J>", function()
		if ls.choice_active() then
			ls.change_choice(1)
		end
	end, { silent = true })
end

return {
	-- Completion
	{
		"hrsh7th/nvim-cmp",
		config = setup_cmp,
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-cmdline",
			"hrsh7th/cmp-buffer",
		},
	},
	-- Snippets
	{
		"L3MON4D3/LuaSnip",
		config = setup_snippets,
		dependencies = {
			"rafamadriz/friendly-snippets",
			"saadparwaiz1/cmp_luasnip",
		},
	},
}
