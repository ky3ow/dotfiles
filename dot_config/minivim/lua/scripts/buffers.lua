local Buffers = {
	buf2nr = {}
}

function Buffers.init()
	Buffers.buf2nr = vim.tbl_map(function(buffer)
			return buffer.bufnr
		end,
		vim.fn.getbufinfo { buflisted = 1 })
end

function Buffers.find(el)
	for idx, value in ipairs(Buffers.buf2nr) do
		if el == value then
			return idx
		end
	end

	return -1
end

function Buffers.remove(el)
	table.remove(Buffers.buf2nr, Buffers.find(el))
end

function Buffers.add(el)
	table.insert(Buffers.buf2nr, el)
end

function Buffers.go_to(idx)
	local buffer = Buffers.buf2nr[idx]
	if buffer then
		vim.cmd.buffer(buffer)
	end
end

function Buffers.set_keymaps()
	for i = 1, 10 do
		local function jump()
			if i > #Buffers.buf2nr then
				Buffers.go_to(#Buffers.buf2nr)
			else
				Buffers.go_to(i)
			end
		end

		local function bufname(idx)
			local bufnr = Buffers.buf2nr[idx]
			if bufnr == nil then
				return "-"
			end
			local name = vim.api.nvim_buf_get_name(bufnr)
			name = vim.fn.fnamemodify(name, ":p:.")
			return name
		end

		vim.keymap.set("n", ("<M-%d>"):format(i % 10), jump, { desc = bufname(i) })
	end
end

local augroup = vim.api.nvim_create_augroup("ky3ow.Buffers", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", {
	group = augroup,
	pattern = "*",
	callback = function()
		Buffers.init()
	end,
})

vim.api.nvim_create_autocmd("BufAdd", {
	group = augroup,
	pattern = "*",
	callback = function(e)
		local listed = vim.bo[e.buf].buflisted
		local bufname = vim.api.nvim_buf_get_name(e.buf)
		if listed and #bufname ~= 0 then
			Buffers.add(e.buf)
		end
	end,
})

vim.api.nvim_create_autocmd("BufDelete", {
	group = augroup,
	pattern = "*",
	callback = function(e)
		Buffers.remove(e.buf)
	end,
})

Buffers.set_keymaps()
vim.keymap.set("n", "<M-->", "<cmd>e #<cr>", { desc = "Go to alernate file" })
