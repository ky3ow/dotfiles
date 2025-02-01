local H = {}

function H.jinja(path, bufnr) ---@diagnostic disable-line: unused-local
	local ft = "jinja"

	if string.find(path, "%.html") then
		ft = "html"
	end
	if string.find(path, "%.yml") or string.find(path, "%.yaml") then
		ft = "yaml"
	end
	return ft
end

vim.filetype.add {
	extension = {
		jinja = H.jinja,
		j2 = H.jinja,
		tf = "terraform",
	},
	pattern = {
		[".*/pack/.*/doc/.*%.txt"] = "help",
	}
}
