MiniDeps.later(function()
	require("mini.pick").setup {
		mappings = {
			choose_marked = "<M-q>",
			refine = "<C-e>",
			refine_marked = "<M-e>",

			mark_down = {
				char = "<M-n>",
				func = function()
					local keys = vim.api.nvim_replace_termcodes("<C-x><C-n>", true, true, true)
					vim.api.nvim_feedkeys(keys, "n", false)
				end
			},
			mark_up = {
				char = "<M-p>",
				func = function()
					local keys = vim.api.nvim_replace_termcodes("<C-x><C-p>", true, true, true)
					vim.api.nvim_feedkeys(keys, "n", false)
				end
			},
		},
	}
	require("mini.extra").setup {}
	require("mini.icons").setup {
		style = "glyph"
	}
	require "mini.diff".setup {}

	local H = {}

	function H.show_with_icons(buf_id, items, query)
		MiniPick.default_show(buf_id, items, query, { show_icons = true })
	end

	function H.full_path(path) return (vim.fn.fnamemodify(path, ':p'):gsub('(.)/$', '%1')) end

	MiniPick.registry.rg = function(local_opts)
		local picker_opts = { source = { cwd = local_opts.cwd, show = H.show_with_icons } }

		local_opts.cwd = nil
		local_opts.command = { "rg", "--files", "--no-follow", "--color=never" }

		if local_opts.all then
			table.insert(local_opts.command, "--hidden")
		end

		return MiniPick.builtin.cli(local_opts, picker_opts)
	end

	MiniPick.registry.rg_live = function(local_opts)
		local cwd = H.full_path(local_opts.cwd or vim.fn.getcwd())
		local exact = local_opts.exact

		local set_items_opts, spawn_opts = { do_match = false, querytick = MiniPick.get_querytick() }, { cwd = cwd }
		local process
		local match = function(_, _, query)
			pcall(vim.uv.process_kill, process)

			local tick = MiniPick.get_querytick()
			if tick == set_items_opts.querytick then return end
			if #query == 0 then return MiniPick.set_picker_items({}, set_items_opts) end

			local command = {
				"rg", "--column", "--line-number", "--no-heading", "--field-match-separator", "\\x00", "--no-follow",
				"--color=never"
			}

			local querystr = table.concat(query)

			if not exact then
				table.insert(command, "--ignore-case")
			end

			table.insert(command, "--")
			table.insert(command, querystr)

			set_items_opts.querytick = tick
			process = MiniPick.set_picker_items_from_cli(command,
				{ set_items_opts = set_items_opts, spawn_opts = spawn_opts })
		end

		return MiniPick.start {
			source = {
				name = string.format("Grep Live(case: %s)", exact and "I" or "i"),
				match = match,
				show = H.show_with_icons,
				items = {},
			},
			mappings = {
				toggle_case = {
					char = "<C-d>",
					func = function()
						exact = not exact
						MiniPick.set_picker_opts({ source = { name = string.format("Grep Live(case: %s)", exact and "I" or "i") } })
						MiniPick.set_picker_query(MiniPick.get_picker_query() or { "" })
					end
				}
			},
		}
	end

	MiniPick.registry.rg_fuzzy = function(local_opts)
		local cwd = H.full_path(local_opts.cwd or vim.fn.getcwd())

		local set_items_opts, spawn_opts = { do_match = false, querytick = MiniPick.get_querytick() }, { cwd = cwd }
		local process
		local match = function(_, _, query)
			pcall(vim.uv.process_kill, process)

			local tick = MiniPick.get_querytick()
			if tick == set_items_opts.querytick then return end
			if #query == 0 then return MiniPick.set_picker_items({}, set_items_opts) end

			local rg = [[rg --column --line-number --no-heading --ignore-case --field-match-separator="$0" --no-follow --color=never]]

			local command = { "sh", "-c" }
			local commands = {}

			local patterns = vim.split(table.concat(query), " ", { trimempty = true })

			for idx, pattern in ipairs(patterns) do
				if idx ~= #patterns then
					table.insert(commands, string.format("%s --files-with-matches -- '%s'", rg, pattern))
				else
					table.insert(commands, string.format("%s -- '%s'", rg, pattern))
				end
			end

			local pipe = table.concat(commands, " | xargs ")
			table.insert(command, pipe)
			table.insert(command, "\\x00")

			vim.notify(vim.inspect(command))

			set_items_opts.querytick = tick
			process = MiniPick.set_picker_items_from_cli(command,
				{ set_items_opts = set_items_opts, spawn_opts = spawn_opts })
		end

		return MiniPick.start {
			source = {
				name = string.format("Grep Text"),
				match = match,
				show = H.show_with_icons,
				items = {},
			},
		}
	end

	-- just proof of concept
	MiniPick.registry.modified = function(local_opts)
		local_opts.scope = "modified"

		local opts = {
			mappings = {
				open = {
					char = '<C-d>',
					func = function()
						local state = MiniPick.get_picker_state()
						local preview_bufnr = state and state.buffers.preview
						if not preview_bufnr then
							return
						end

						return MiniDiff.toggle_overlay(preview_bufnr)
					end
				},
			},
			source = {
				preview = function(buf_id, path, opts)
					local win_id = vim.fn.bufwinid(buf_id)
					if win_id == -1 then return end
					vim.wo[win_id].number = true

					local ok, _ = pcall(vim.api.nvim_buf_set_name, buf_id, path)
					if ok then
						MiniDiff.enable(buf_id)
						pcall(MiniDiff.toggle_overlay, buf_id)
					end

					return MiniPick.default_preview(buf_id, path, opts)
				end
			},
		}

		return MiniExtra.pickers.git_files(local_opts, opts)
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
	vim.keymap.set("n", "<leader><leader>", "<cmd>Pick buffers<cr>", { desc = "[S]earch [b]uffers" })

	vim.keymap.set("n", "<leader>sc", "<cmd>Pick list scope='quickfix'<cr>", { desc = "[S]earch qui[c]fix" })
	vim.keymap.set("n", "<leader>sm", "<cmd>Pick modified<cr>", { desc = "[S]earch [g]it" })
	vim.keymap.set("n", "<leader>sd", "<cmd>Pick diagnostic scope='current'<cr>", { desc = "[S]earch [d]ignostic" })

	vim.keymap.set("n", "<leader>spf", [[<cmd>execute 'Pick files cwd="' . g:mini_deps . '"'<cr>]],
		{ desc = "[S]earch [P]ackage [f]iles" })
	vim.keymap.set("n", "<leader>spg", [[<cmd>execute 'Pick grep_live cwd="' . g:mini_deps . '"'<cr>]],
		{ desc = "[S]earch [P]ackage [g]rep" })

	vim.keymap.set("n", "<leader>sw", "<cmd>Pick grep_word<cr>", { desc = "[S]earch [w]ord" })
	vim.keymap.set("n", "<leader>s/", "<cmd>Pick buf_lines scope='current'<cr>", { desc = "[S]earch [/]lines" })

	vim.ui.select = MiniPick.ui_select
end)
