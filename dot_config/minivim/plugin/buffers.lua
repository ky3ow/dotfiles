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
	local low, high = 1, #Buffers.buf2nr
	while low <= high do
		local mid = math.floor((low + high) / 2)
		if Buffers.buf2nr[mid] == el then
			return mid
		elseif Buffers.buf2nr[mid] < el then
			low = mid + 1
		else
			high = mid - 1
		end
	end

	return -1
end

function Buffers.remove(el)
	table.remove(Buffers.buf2nr, Buffers.find(el))
end

function Buffers.add(el)
	local low, high = 1, #Buffers.buf2nr
	while low <= high do
		local mid = math.floor((low + high) / 2)
		if Buffers.buf2nr[mid] < el then
			low = mid + 1
		else
			high = mid - 1
		end
	end
	table.insert(Buffers.buf2nr, low, el)
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
		if listed then
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
		if vim.g.goto_notify then
			vim.notify(("Switched to edge[%d]"):format(opts.args))
		end
		Buffers.go_to(#Buffers.buf2nr)
	else
		Buffers.go_to(opts.args)
	end
end, {
	nargs = 1,
})
