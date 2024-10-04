-- [[ Basic Keymaps ]]
return {
	setup = function(_)
		vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
		vim.keymap.set("n", "<C-e>", "<Nop>")

		vim.keymap.set("n", "<esc>", vim.cmd.nohlsearch, { desc = "Clear highlight" })

		vim.keymap.set("n", "<leader>e", vim.cmd.Ex, { desc = "[E]xplorer" })

		-- Remap for dealing with word wrap
		vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
		vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

		-- Diagnostic keymaps
		vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
		vim.keymap.set("n", "<leader>d", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
		vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

		-- Center jumping
		vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down" })
		vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up" })
		vim.keymap.set("n", "n", "nzzzv", { desc = "Next search" })
		vim.keymap.set("n", "N", "Nzzzv", { desc = "Prev search" })

		-- Navigate buffers
		vim.keymap.set("n", "]b", vim.cmd.bnext, { desc = "Next [B]uffer" })
		vim.keymap.set("n", "[b", vim.cmd.bprevious, { desc = "Prev [B]uffer" })

		-- Navigate tabs
		vim.keymap.set("n", "]t", "gt", { desc = "Next [T]ab" })
		vim.keymap.set("n", "[t", "gT", { desc = "Prev [T]ab" })

		-- Stay in indent mode
		vim.keymap.set("v", "<", "<gv", { desc = "Outdent" })
		vim.keymap.set("v", ">", ">gv", { desc = "Indent" })

		-- Move text
		vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move down" })
		vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move up" })

		-- Clipboard
		vim.keymap.set({ "n", "x" }, "gy", '"+y', { desc = "Copy to clip" })
		vim.keymap.set("n", "gp", '"+p', { desc = "Paste from clipboard" })
		vim.keymap.set("x", "gp", '"+P', { desc = "Paste from clipboard" })

		-- Substitute
		vim.keymap.set("n", "<leader>rW", ":%s/<C-r><C-w>//gI<Left><Left><Left>", { desc = "Replace WORD" })
		vim.keymap.set("n", "<leader>rw", ":%s/\\<<C-r><C-w>\\>//gI<Left><Left><Left>", { desc = "Replace word" })
		vim.keymap.set("n", "<leader>rs", [[:%s/\v]], { desc = "[S]earch and [r]eplace" })
		vim.keymap.set("x", "s", [[:s/\v]], { desc = "Substitute" })

		-- Regional search
		vim.keymap.set("n", "g/", [[/\%<c-r>=line('.')<cr>l]], { desc = "Search inside current line" })
		vim.keymap.set("x", "g/", '<esc>/\\%V', { desc = "Search inside selection" })

		vim.keymap.set('n', 'gV', '"`[" . strpart(getregtype(), 0, 1) . "`]"', { expr = true, replace_keycodes = false, desc = 'Visually select changed text' })
		-- Instead which-key nested keymap to make beautiful
		-- vim.keymap.set(
		-- 	"x",
		-- 	"<leader>@",
		-- 	[[:<C-u>echo "macro: @" | execute "'<,'>normal @".getcharstr()<CR>]],
		-- 	{ desc = "Visual at" }
		-- )
	end,
}
