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

vim.api.nvim_create_user_command("Goto", function(opts)
	opts.args = tonumber(opts.args)
	if opts.args > #Buffers.buf2nr then
		Buffers.go_to(#Buffers.buf2nr)
	else
		Buffers.go_to(opts.args)
	end
end, {
	nargs = 1,
})
