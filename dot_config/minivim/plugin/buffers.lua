vim.api.nvim_create_user_command("Goto", function(opts)
	local buffers = vim.fn.getbufinfo { buflisted = 1 }
	local target = buffers[tonumber(opts.args)] or buffers[#buffers]

	vim.cmd.buffer(target.bufnr)
end, {
	nargs = 1,
})

for i = 1, 10 do
	vim.keymap.set("n", ("<M-%d>"):format(i % 10), ("<cmd>Goto %d<cr>"):format(i),
		{ desc = ("Go to buffer %d"):format(i) })
end
vim.keymap.set("n", "<M-->", "<cmd>e #<cr>", { desc = "Go to alernate file" })
