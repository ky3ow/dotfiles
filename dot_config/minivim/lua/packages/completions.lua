MiniDeps.later(function()
	MiniDeps.add "rafamadriz/friendly-snippets"
	require("mini.completion").setup {
		delay = {
			completion = 10,
			info = 200,
			signature = 200,
		},
		lsp_completion = {
			source_func = "omnifunc",
			auto_setup = false,
		},
	}
	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("ky3ow.completion", { clear = true }),
		callback = function(args)
			vim.bo[args.buf].omnifunc = 'v:lua.MiniCompletion.completefunc_lsp'
		end
	})
	-- @TODO look into MiniSnippets.default_insert() to add dynamic node logic?(if i ever need that?)
	local gen_loader = require("mini.snippets").gen_loader
	require("mini.snippets").setup {
		snippets = {
			gen_loader.from_lang(),
			gen_loader.from_runtime("globals.json"),
			gen_loader.from_runtime("globals.lua"),
		},
	}
end)
