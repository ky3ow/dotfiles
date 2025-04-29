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

local config_location = vim.fn.stdpath "config"

vim.filetype.add {
	extension = {
		jinja = H.jinja,
		j2 = H.jinja,
		tf = "terraform",
		tfvars = "terraform",
	},
	filename = {
		[config_location .. "/mini-deps-snap"] = "lua"
	},
	pattern = {
		[".*/pack/.*/doc/.*%.txt"] = "help",
	},
}
