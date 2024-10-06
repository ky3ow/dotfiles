return {
	setup = function(_)
		local function set_fold_hl()
			local cl = vim.api.nvim_get_hl(0, { name = "Number" })

			vim.api.nvim_set_hl(0, "FoldedText", { bg = cl.bg, fg = cl.fg })
		end

		set_fold_hl()
		vim.api.nvim_create_autocmd("ColorScheme", {
			callback = set_fold_hl,
		})


		vim.opt.foldtext = 'v:lua.require("config.folds").fold_text()'
		vim.opt.foldopen:remove('block')
	end,


	fold_text = function()
		local lines = vim.v.foldend - vim.v.foldstart + 1

		local start_content = table.concat(vim.fn.getbufline(vim.api.nvim_get_current_buf(), vim.v.foldstart))
		local indent, text = start_content:match("^(%s*)(%S.*)")
		local spaces = string.rep(" ", vim.fn.strdisplaywidth(indent))

		local marker =
			{ { spaces .. "> ", "FoldedText" }, { text }, { "[" }, { tostring(lines), "FoldedText" }, { "]" }, { "...", "FoldedText" } }
		return marker
	end,
}
