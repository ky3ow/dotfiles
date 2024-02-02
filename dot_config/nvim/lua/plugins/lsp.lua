local servers = {
	-- clangd = {},
	-- gopls = {},
	-- pyright = {},
	-- rust_analyzer = {},
	-- tsserver = {},
	-- html = { filetypes = { 'html', 'twig', 'hbs'} },

	lua_ls = {
		Lua = {
			workspace = { checkThirdParty = false },
			telemetry = { enable = false },
			diagnostics = {
				disable = { 'missing-fields' },
				globals = {},
			}
		},
	},
}

local function on_attach(_, bufnr)
	local telescope = require('telescope.builtin')
	local nmap = function(keys, func, desc)
		if desc then
			desc = 'LSP: ' .. desc
		end

		vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
	end

	nmap('<leader>lr', vim.lsp.buf.rename, '[L]SP [R]ename')
	nmap('<leader>la', vim.lsp.buf.code_action, '[L]sp Code [A]ction')
	nmap('<leader>lf', vim.lsp.buf.format, '[L]SP [F]ormat')
	nmap('<leader>ls', telescope.lsp_document_symbols, '[L]SP document [S]ymbols')

	nmap('gd', telescope.lsp_definitions, '[G]oto [D]efinition')
	nmap('gr', telescope.lsp_references, '[G]oto [R]eferences')
	nmap('gI', telescope.lsp_implementations, '[G]oto [I]mplementation')
	nmap('<leader>D', telescope.lsp_type_definitions, 'Type [D]efinition')

	nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
	nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

	nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
	nmap('<leader>ws', telescope.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
	nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
	nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
	nmap('<leader>wl', function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, '[W]orkspace [L]ist Folders')

	vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
		vim.lsp.buf.format()
	end, { desc = 'Format current buffer with LSP' })
end

local function setup()
	local cmp_lsp = require('cmp_nvim_lsp')
	local capabilities = vim.tbl_deep_extend(
		"force",
		{},
		vim.lsp.protocol.make_client_capabilities(),
		cmp_lsp.default_capabilities()
	)

	require('mason').setup()
	require('mason-lspconfig').setup({
		ensure_installed = vim.tbl_keys(servers),
		handlers = {
			function(server_name)
				require('lspconfig')[server_name].setup {
					capabilities = capabilities,
					on_attach = on_attach,
					settings = servers[server_name],
					filetypes = (servers[server_name] or {}).filetypes,
				}
			end,
		}
	})

	local cmp = require('cmp')
	local ls = require 'luasnip'
	cmp.setup {
		snippet = {
			expand = function(args)
				ls.lsp_expand(args.body)
			end,
		},
		completion = { completeopt = 'menu,menuone,noinsert', },
		mapping = cmp.mapping.preset.insert {
			['<C-n>'] = cmp.mapping.select_next_item(),
			['<C-p>'] = cmp.mapping.select_prev_item(),
			['<C-y>'] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true, },
			['<C-Space>'] = cmp.mapping.complete(),
		},
		sources = {
			{ name = 'nvim_lsp' },
			{ name = 'luasnip' },
			{ name = 'path' },
			{ name = 'buffer' },
		},
	}

	-- ls.filetype_extend("javascript", { "jsdoc" })
	vim.keymap.set({ "i", "s" }, "<C-L>", function() ls.jump(1) end, { silent = true })
	vim.keymap.set({ "i", "s" }, "<C-H>", function() ls.jump(-1) end, { silent = true })
end

return {
	{
		'neovim/nvim-lspconfig',
		config = setup,
		dependencies = {
			'williamboman/mason.nvim',
			'williamboman/mason-lspconfig.nvim',
			{ 'folke/neodev.nvim', opts = {} },
			{ 'j-hui/fidget.nvim', opts = {} },
			-- Completion
			'hrsh7th/nvim-cmp',
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-cmdline',
			'hrsh7th/cmp-buffer',
			-- For formatters/linters
			{
				'nvimtools/none-ls.nvim',
				config = function()
					local null_ls = require('null-ls')
					null_ls.setup({
						sources = {
							null_ls.builtins.formatting.stylua,
						}
					})
				end,
			},
			-- Snippets
			'L3MON4D3/LuaSnip',
			'saadparwaiz1/cmp_luasnip',
		},
	},
}
