return {
	setup = function(_)
		-- extensions->filetype mapping
		-- filetype can be function(path, bufnr) -> ft
		local filetypes = {
			[{ "jinja", "j2" }] = function(path, _)
				local ft = "jinja"

				if string.find(path, "%.html") then
					ft = "html"
				end
				if string.find(path, "%.yml") or string.find(path, "%.yaml") then
					ft = "yaml"
				end
				return ft
			end,
		}

		local extension_table = {}
		for extensions, ft_func in pairs(filetypes) do
			for _, ext in ipairs(extensions) do
				extension_table[ext] = ft_func
			end
		end

		vim.filetype.add({
			extension = extension_table,
		})
	end,
}
