local later = require("mini.deps").later

later(function()
	require("mini.pick").setup {
		mappings = {
			choose_marked = "<C-j>",
			refine_marked = "<C-q>",

			mark_down = {
				char = "<C-e>",
				func = function()
					local keys = vim.api.nvim_replace_termcodes("<C-x><C-n>", true, true, true)
					vim.api.nvim_feedkeys(keys, "n", false)
				end
			},
		},
	}
	require("mini.extra").setup {}
	require("mini.icons").setup {
		style = "glyph"
	}

	local H = {}

	function H.show_with_icons(buf_id, items, query)
		MiniPick.default_show(buf_id, items, query, { show_icons = true })
	end

	MiniPick.registry.rg = function(local_opts)
		local picker_opts = { source = { cwd = local_opts.cwd, show = H.show_with_icons } }

		local_opts.cwd = nil
		local_opts.command = { "rg", "--files", "--no-follow", "--color=never" }

		if local_opts.all then
			table.insert(local_opts.command, "--hidden")
		end

		return MiniPick.builtin.cli(local_opts, picker_opts)
	end

	MiniPick.registry.grep_word = function()
		local word = vim.fn.expand("<cword>")
		return MiniPick.builtin.grep({
			pattern = word
		}, {
			source = {
				name = string.format('Grep: <%s>', word)
			},
		})
	end

	MiniPick.registry.files = function(local_opts)
		local open_selected = function()
			local matches = MiniPick.get_picker_matches()
			if not matches then
				return
			end

			local selected = #matches.marked > 0 and matches.marked or { matches.current }

			vim.schedule(function()
				for _, path in ipairs(selected) do
					MiniPick.default_choose(path)
				end
			end)

			return MiniPick.stop()
		end

		local opts = {
			source = {
				cwd = local_opts.cwd,
			},
			mappings = {
				open = { char = '<C-o>', func = open_selected },
			}
		}
		local_opts.cwd = nil
		return MiniPick.builtin.files(local_opts, opts)
	end

	MiniPick.registry.grep_live = function(local_opts)
		local opts = { source = { cwd = local_opts.cwd } }
		local_opts.cwd = nil
		return MiniPick.builtin.grep_live(local_opts, opts)
	end

	MiniPick.registry.highlights = function()
		local highlights = vim.api.nvim_get_hl(0, {})
		local hls = {}
		for name, value in pairs(highlights) do
			table.insert(hls, { name = name, value = value })
		end
		return MiniPick.start({
			source = {
				items = hls,
				show = function(buf_id, items_arr, _ --[[ query ]])
					local lines = vim.tbl_map(function(hl)
						return string.format("%s: %s", hl.name, vim.inspect(hl.value, { newline = " ", indent = "" }))
					end, items_arr)
					vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
					for index, hlgroup in ipairs(items_arr) do
						vim.api.nvim_buf_add_highlight(buf_id, -1, hlgroup.name, index - 1, 0, #hlgroup.name)
					end
				end
			},
		})
	end

	MiniPick.registry.buffers = function(local_opts)
		local wipeout_cur = function()
			local matches = MiniPick.get_picker_matches()
			if not matches then
				return
			end

			local deleted = vim.tbl_map(function(v) return v.bufnr end,
				#matches.marked > 0 and matches.marked or { matches.current })
			local remaining = vim.tbl_filter(function(m)
				return not vim.tbl_contains(deleted, m.bufnr, {})
			end, matches.all)

			for _, buffer in ipairs(deleted) do
				vim.api.nvim_buf_delete(buffer, {})
			end
			MiniPick.set_picker_items(remaining)
		end
		local buffer_mappings = { wipeout = { char = '<C-d>', func = wipeout_cur } }
		MiniPick.builtin.buffers(local_opts, { mappings = buffer_mappings })
	end

	vim.keymap.set("n", "<leader>sf", "<cmd>Pick files<cr>", { desc = "[S]earch [f]iles" })
	vim.keymap.set("n", "<leader>sg", "<cmd>Pick grep_live<cr>", { desc = "[S]earch [g]rep" })
	vim.keymap.set("n", "<leader>sG", "<cmd>Pick grep<cr>", { desc = "[S]earch [g]rep(non interactive)" })
	vim.keymap.set("n", "<leader>sh", "<cmd>Pick help<cr>", { desc = "[S]earch [h]elp" })
	vim.keymap.set("n", "<leader>sb", "<cmd>Pick buffers<cr>", { desc = "[S]earch [b]uffers" })

	vim.keymap.set("n", "<leader>sc", "<cmd>Pick list scope='quickfix'<cr>", { desc = "[S]earch qui[c]fix" })
	vim.keymap.set("n", "<leader>sm", "<cmd>Pick git_files scope='modified'<cr>", { desc = "[S]earch [g]it" })
	vim.keymap.set("n", "<leader>sd", "<cmd>Pick diagnostic scope='current'<cr>", { desc = "[S]earch [d]ignostic" })

	vim.keymap.set("n", "<leader>spf", [[<cmd>execute 'Pick files cwd="' . g:mini_deps . '"'<cr>]],
		{ desc = "[S]earch [P]ackage [f]iles" })
	vim.keymap.set("n", "<leader>spg", [[<cmd>execute 'Pick grep_live cwd="' . g:mini_deps . '"'<cr>]],
		{ desc = "[S]earch [P]ackage [g]rep" })

	vim.keymap.set("n", "<leader>sw", "<cmd>Pick grep_word<cr>", { desc = "[S]earch [w]ord" })
	vim.keymap.set("n", "<leader>s/", "<cmd>Pick buf_lines scope='current'<cr>", { desc = "[S]earch [/]lines" })

	vim.ui.select = MiniPick.ui_select
end)
