return {
	setup = function(_)
		FoldText = function()
			local lines = vim.v.foldend - vim.v.foldstart + 1
			local width = vim.api.nvim_win_get_width(0) - vim.fn.getwininfo(vim.fn.win_getid())[1].textoff

			local start_content = table.concat(vim.fn.getbufline(vim.api.nvim_get_current_buf(), vim.v.foldstart))
			local indent, text = start_content:match("^(%s*)(%S.*)")
			local spaces = string.rep(" ", vim.fn.strdisplaywidth(indent) + 1)

			local marker = spaces .. "[+]----| " .. text .. " |----[" .. lines .. "]"
			return marker .. string.rep(" ", width - vim.fn.strchars(marker))
		end

		vim.o.foldtext = "v:lua.FoldText()"
	end,
}
