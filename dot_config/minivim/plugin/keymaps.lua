-- [[ Basic Keymaps ]]
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

vim.keymap.set("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear highlight" })
vim.keymap.set("n", "<leader>d", "<cmd>Ex<cr>", { desc = "Directory" })
vim.keymap.set({ "i", "n" }, "<A-e>", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set("n", "[d", "<cmd>lua vim.diagnostic.jump{count=-1}<cr>", { desc = "Go to previous diagnostic message" })
vim.keymap.set("n", "]d", "<cmd>lua vim.diagnostic.jump{count=1}<cr>", { desc = "Go to next diagnostic message" })

-- Quickfix
vim.keymap.set("n", "]c", "<cmd>cnext<cr>", { desc = "Go to next quickfix entry message" })
vim.keymap.set("n", "[c", "<cmd>cprevious<cr>", { desc = "Go to previous quickfix entry message" })
vim.keymap.set("n", "]f", "<cmd>cnfile<cr>", { desc = "Go to next quickfix file" })
vim.keymap.set("n", "[f", "<cmd>cpfile<cr>", { desc = "Go to previous quickfix file" })

-- Center jumping
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up" })
vim.keymap.set("n", "n", "nzzzv", { desc = "Next search" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Prev search" })

-- Navigate buffers
vim.keymap.set("n", "]b", "<cmd>bnext<cr>", { desc = "Next Buffer" })
vim.keymap.set("n", "[b", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })

-- Navigate tabs
vim.keymap.set("n", "]t", "gt", { desc = "Next Tab" })
vim.keymap.set("n", "[t", "gT", { desc = "Prev Tab" })

-- Stay in indent mode
vim.keymap.set("v", "<", "<gv", { desc = "Outdent" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent" })

-- Move text
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move down" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move up" })

-- Clipboard
vim.keymap.set({ "n", "x" }, "gy", '"+y', { desc = "Copy to clip" })
vim.keymap.set({ "n", "x" }, "gp", '"+p', { desc = "Paste from clipboard" })

-- Regional search
vim.keymap.set("n", "g/", [[/\%<c-r>=line('.')<cr>l]], { desc = "Search inside current line" })
vim.keymap.set("x", "g/", [[<esc>/\%V]], { desc = "Search inside selection" })

vim.keymap.set(
	"n",
	"gV",
	[["`[" . strpart(getregtype(), 0, 1) . "`]"]],
	{ expr = true, replace_keycodes = false, desc = "Visually select changed text" }
)

-- Instead which-key nested keymap to make beautiful
vim.keymap.set(
	"x",
	"@",
	[[mode() == 'V' ? ':normal! @'.getcharstr().'<CR>' : '@']],
	{ desc = "Visual at", expr = true }
)

vim.keymap.set({ "n", "i" }, "<M-d>", "<cmd>silent! write | bdelete!<cr>", { desc = "Delete buffer(save before)" })

-- mini.surround
vim.keymap.set({ "n", "x" }, "s", "<nop>")

vim.g.luaop = function(type)
	type = type or ""
	if type == "" then
		vim.o.operatorfunc = "v:lua.vim.g.luaop"
		return "g@"
	end
	local start_line, end_line = vim.fn.line "'[", vim.fn.line "']"
	local start_col, end_col
	if type == "line" then
		start_col, end_col = 1, vim.fn.strlen(vim.fn.getline(end_line))
	else
		start_col, end_col = vim.fn.col "'[", vim.fn.col "']"
	end

	local text = vim.api.nvim_buf_get_text(0, start_line - 1, start_col - 1, end_line - 1, end_col, {})

	vim.notify(vim.inspect{ start_line - 1, start_col - 1, end_line - 1, end_col }, vim.log.levels.DEBUG)
	vim.notify("TEXT:" .. vim.inspect(text), vim.log.levels.DEBUG)

	local func, parse_err = load(table.concat(text, "\n"))
	if func then
		local ok, res = pcall(func)
		if not res and ok then return end
		res = vim.inspect(res)
		local level = (not ok or res:find('^"*%[string') ~= nil) and vim.log.levels.ERROR or vim.log.levels.INFO
		vim.notify("Runtime: "  .. res, level)
	else
		vim.notify("Parse error: " .. parse_err, vim.log.levels.ERROR)
	end
end

vim.keymap.set({ "n", "x" }, "g=", vim.g.luaop, { desc = "Go lua", expr = true })
vim.keymap.set("n", "g==", "<cmd>source %<cr>", { desc = "Go lua" })
