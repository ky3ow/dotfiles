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
		xsh = "python",
	},
	filename = {
		[config_location .. "/mini-deps-snap"] = "lua",
		[".xonshrc"] = "python",
	},
	pattern = {
		[".*/pack/.*/doc/.*%.txt"] = "help",
		["/.*%.azure.*"] = "azurepipelines",
		["/.*%.gh.*"] = "gh",
	},
}

vim.treesitter.language.register("yaml", "azurepipelines")
vim.treesitter.language.register("yaml", "gh")

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "azurepipelines", "gh" },
	callback = function(args)
		vim.bo.shiftwidth = 2
		vim.bo.tabstop = 2
		vim.bo.expandtab = true

		vim.api.nvim_create_autocmd("InsertLeave", {
			buffer = args.buf,
			command = "retab",
		})
	end,
})
