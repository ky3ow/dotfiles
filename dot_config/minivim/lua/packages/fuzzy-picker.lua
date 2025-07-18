MiniDeps.later(function()
	local H = {}

	---@param keys string
	function H.map_keys(keys)
		return function()
			keys = vim.api.nvim_replace_termcodes(keys, true, true, true)
			vim.api.nvim_feedkeys(keys, "n", false)
		end
	end

	function H.show_with_icons(buf_id, items, query)
		MiniPick.default_show(buf_id, items, query, { show_icons = true })
	end

	function H.full_path(path)
		-- handle root dir
		return (vim.fn.fnamemodify(path, ":p"):gsub("(.)/$", "%1"))
	end

	-- TODO! review pickers
	require("mini.pick").setup {
		mappings = {
			choose_marked = "<C-j>",
			choose = "<CR>",
			refine = "<C-Space>",
			refine_marked = "<M-j>",

			scroll_right = "<M-l>",
			scroll_left = "<M-h>",

			mark_down = {
				char = "<M-n>",
				func = H.map_keys "<C-x><C-n>",
			},
			mark_up = {
				char = "<M-p>",
				func = H.map_keys "<C-x><C-p>",
			},
		},
	}
	require("mini.extra").setup {}
	require("mini.diff").setup {}

	---@class ModuleState
	---@field name string
	---@field value string[]

	---@class Module
	---@field _idx integer
	---@field states ModuleState[]
	local Module = {
		_idx = 1,
		states = {},
	}

	---@return ModuleState
	function Module.cycle(self)
		self._idx = self._idx + 1
		if self._idx > #self.states then
			self._idx = 1
		end
		return self:current()
	end

	---@return ModuleState
	function Module.current(self)
		return self.states[self._idx]
	end

	---@param states ModuleState[]
	---@return Module
	function Module.new(states)
		local self = setmetatable({}, {
			__index = Module,
		})
		self.states = states
		return self
	end

	---@class State
	---@field _name string
	---@field local_opts table
	---@field modules table<string, Module>
	local State = {
		modules = {},
	}

	---@param name string
	---@param local_opts table
	---@param modules table<string, Module>
	---@return State
	function State.new(name, local_opts, modules)
		local self = setmetatable({}, {
			__index = State,
		})
		self._name = name
		self.local_opts = local_opts
		self.modules = modules
		return self
	end

	---@return string
	function State.name(self)
		local names = {}
		for _, module in pairs(self.modules) do
			table.insert(names, module:current().name)
		end
		return ("%s(:%s:)"):format(self._name, table.concat(names, ":"))
	end

	function State.command(self)
		return vim.iter(self.modules):fold(vim.deepcopy(self.local_opts.command), function(acc, _, val)
			return vim.list_extend(acc, val:current().value)
		end)
	end

	function State.wrap_cycle(self, module)
		return function()
			self.modules[module]:cycle()
			local command = self:command()

			MiniPick.set_picker_opts {
				source = {
					name = self:name(),
				},
			}
			MiniPick.set_picker_items_from_cli(command, self.local_opts.spawn_opts)
		end
	end

	MiniPick.registry.narrow = function(local_opts)
		local cwd = H.full_path(local_opts.cwd or vim.fn.getcwd())
		local exact = local_opts.exact
		local show_files, just_narrowed = false, false
		local remembered_files, remembered_queries = {}, {}

		local function name()
			return string.format("Grep Live(%s) State(%s)", exact and "I" or "i", table.concat(remembered_queries, ","))
		end

		local function widen()
			if not MiniPick.get_picker_query() then
				return
			end

			if #remembered_files == 0 then
				MiniPick.set_picker_opts { source = { name = name() } }
				return MiniPick.set_picker_query { "" }
			end

			table.remove(remembered_files, #remembered_files)
			local last_query = table.remove(remembered_queries, #remembered_queries)

			MiniPick.set_picker_opts { source = { name = name() } }
			MiniPick.set_picker_query(vim.split(last_query, ""))
		end

		local function narrow()
			local items, query = MiniPick.get_picker_items(), MiniPick.get_picker_query()
			if not items or #items == 0 or not query or (#query == 1 and #query[1] == 0) then
				return
			end

			local seen = {}
			local files = vim.tbl_map(function(item)
				return vim.split(item, "\x00")[1]
			end, items)
			files = vim.tbl_filter(function(file)
				local unique = not vim.tbl_contains(seen, file)
				if unique then
					table.insert(seen, file)
				end
				return unique
			end, files)

			table.insert(remembered_files, files)
			table.insert(remembered_queries, table.concat(query))

			show_files, just_narrowed = true, true
			MiniPick.set_picker_opts { source = { name = name() } }
			MiniPick.set_picker_query { "" }
		end

		local set_items_opts, spawn_opts = { do_match = false, querytick = MiniPick.get_querytick() }, { cwd = cwd }
		local process
		local match = function(_, _, query)
			pcall(vim.uv.process_kill, process)

			local tick = MiniPick.get_querytick()
			if tick == set_items_opts.querytick then
				return
			end
			if #query == 0 then
				return MiniPick.set_picker_items({}, set_items_opts)
			end

			local command = {
				"rg",
				"--column",
				"--line-number",
				"--no-heading",
				"--field-match-separator",
				"\\x00",
				"--no-follow",
				"--color=never",
				"--with-filename",
				"-e",
				table.concat(query),
			}

			if not exact then
				table.insert(command, "--ignore-case")
			end

			if show_files then
				table.insert(command, "--files-with-matches")
			end

			vim.list_extend(command, remembered_files[#remembered_files] or {})

			if just_narrowed and show_files then
				just_narrowed, show_files = false, false
			end
			set_items_opts.querytick = tick
			process = MiniPick.set_picker_items_from_cli(
				command,
				{ set_items_opts = set_items_opts, spawn_opts = spawn_opts }
			)
		end

		return MiniPick.start {
			source = {
				name = name(),
				match = match,
				show = H.show_with_icons,
				items = {},
			},
			mappings = {
				delete_word = "<M-w>",
				unique_files = {
					char = "<C-l>",
					func = function()
						show_files = not show_files
						MiniPick.set_picker_opts { source = { name = name() } }
						MiniPick.set_picker_query(MiniPick.get_picker_query() or { "" })
					end,
				},
				toggle_case = {
					char = "<C-d>",
					func = function()
						exact = not exact
						MiniPick.set_picker_opts { source = { name = name() } }
						MiniPick.set_picker_query(MiniPick.get_picker_query() or { "" })
					end,
				},
				narrow = { char = "<Space>", func = narrow },
				widen = { char = "<C-w>", func = widen },
			},
		}
	end

	MiniPick.registry.rg_files = function(local_opts)
		local state = State.new("Find files", local_opts, {
			hidden = Module.new {
				{
					name = "l",
					value = {},
				},
				{
					name = "h",
					value = { "--hidden" },
				},
				{
					name = "H",
					value = { "--hidden", "--no-ignore" },
				},
			},
		})

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

		local cwd = H.full_path(local_opts.cwd or vim.fn.getcwd())

		local_opts.command = {
			"rg",
			"--files",
			"--no-follow",
			"--color=never",
			"--field-match-separator",
			"\\x00",
		}
		local_opts.spawn_opts = { cwd = cwd }

		local picker_opts = {
			source = { show = H.show_with_icons, name = state:name(), cwd = cwd },
			mappings = {
				open = { char = "<C-o>", func = open_selected },
				cycle_hidden = { char = "<C-d>", func = state:wrap_cycle "hidden" },
			},
		}

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
			if tick == set_items_opts.querytick then
				return
			end
			if #query == 0 then
				return MiniPick.set_picker_items({}, set_items_opts)
			end

			local command = {
				"rg",
				"--column",
				"--line-number",
				"--no-heading",
				"--field-match-separator",
				"\\x00",
				"--no-follow",
				"--color=never",
			}

			local querystr = table.concat(query)

			if not exact then
				table.insert(command, "--ignore-case")
			end

			table.insert(command, "--")
			table.insert(command, querystr)

			set_items_opts.querytick = tick
			process = MiniPick.set_picker_items_from_cli(
				command,
				{ set_items_opts = set_items_opts, spawn_opts = spawn_opts }
			)
		end

		return MiniPick.start {
			source = {
				name = string.format("Grep Live(case: %s)", exact and "I" or "i"),
				match = match,
				show = H.show_with_icons,
				items = {},
				cwd = cwd,
			},
			mappings = {
				toggle_case = {
					char = "<C-d>",
					func = function()
						exact = not exact
						MiniPick.set_picker_opts {
							source = { name = string.format("Grep Live(case: %s)", exact and "I" or "i") },
						}
						MiniPick.set_picker_query(MiniPick.get_picker_query() or { "" })
					end,
				},
			},
		}
	end

	-- just proof of concept
	MiniPick.registry.modified = function(local_opts)
		local_opts.scope = "modified"

		local opts = {
			mappings = {
				open = {
					char = "<C-d>",
					func = function()
						local state = MiniPick.get_picker_state()
						local preview_bufnr = state and state.buffers.preview
						if not preview_bufnr then
							return
						end
						local ok, _ = pcall(MiniDiff.toggle_overlay, preview_bufnr)
						if not ok then
							vim.notify "can't do that when file open"
						end
					end,
				},
			},
			source = {
				preview = function(buf_id, path, opts)
					local win_id = vim.fn.bufwinid(buf_id)
					if win_id == -1 then
						return
					end
					vim.wo[win_id].number = true

					local ok, _ = pcall(vim.api.nvim_buf_set_name, buf_id, path)
					if ok then
						MiniDiff.enable(buf_id)
						pcall(MiniDiff.toggle_overlay, buf_id)
					end

					return MiniPick.default_preview(buf_id, path, opts)
				end,
			},
		}

		return MiniExtra.pickers.git_files(local_opts, opts)
	end

	MiniPick.registry.grep_word = function()
		local word = vim.fn.expand "<cword>"
		return MiniPick.builtin.grep({
			pattern = word,
		}, {
			source = {
				name = string.format("Grep: <%s>", word),
			},
		})
	end

	MiniPick.registry.highlights = function()
		local highlights = vim.iter(vim.api.nvim_get_hl(0, {}))
			:map(function(name, hl)
				return { name = name, value = hl }
			end)
			:totable()
		return MiniPick.start {
			source = {
				items = highlights,
				show = function(buf_id, items_arr, query) ---@diagnostic disable-line: unused-local
					local lines = vim.tbl_map(function(hl)
						return string.format("%s: %s", hl.name, vim.inspect(hl.value, { newline = " ", indent = "" }))
					end, items_arr)
					vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
					for index, hlgroup in ipairs(items_arr) do
						vim.api.nvim_buf_set_extmark(
							buf_id,
							vim.api.nvim_create_namespace "",
							index - 1,
							0,
							{ end_col = #hlgroup.name, hl_group = hlgroup.name }
						)
					end
				end,
			},
		}
	end

	MiniPick.registry.buffers = function(local_opts)
		local wipeout = function()
			local matches = MiniPick.get_picker_matches()
			if not matches then
				return
			end

			local deleted = vim.tbl_map(function(v)
				return v.bufnr
			end, #matches.marked > 0 and matches.marked or { matches.current })
			local remaining = vim.tbl_filter(function(m)
				return not vim.tbl_contains(deleted, m.bufnr, {})
			end, matches.all)

			for _, buffer in ipairs(deleted) do
				vim.api.nvim_buf_delete(buffer, {})
			end
			MiniPick.set_picker_items(remaining)
		end
		local buffer_mappings = { wipeout = { char = "<C-d>", func = wipeout } }
		MiniPick.builtin.buffers(local_opts, { mappings = buffer_mappings })
	end

	vim.keymap.set("n", "<leader>sf", "<cmd>Pick rg_files<cr>", { desc = "Search files" })
	vim.keymap.set("n", "<leader>sg", "<cmd>Pick rg_live<cr>", { desc = "Search grep" })
	vim.keymap.set("n", "<leader>sG", "<cmd>Pick grep<cr>", { desc = "Search grep(non interactive)" })
	vim.keymap.set("n", "<leader>sh", "<cmd>Pick help<cr>", { desc = "Search help" })
	vim.keymap.set("n", "<leader><leader>", "<cmd>Pick buffers<cr>", { desc = "Search buffers" })

	vim.keymap.set("n", "<leader>sc", "<cmd>Pick list scope='quickfix'<cr>", { desc = "Search quicfix" })
	vim.keymap.set("n", "<leader>sm", "<cmd>Pick modified<cr>", { desc = "Search git" })
	vim.keymap.set("n", "<leader>sn", "<cmd>Pick narrow<cr>", { desc = "Search narrowing" })
	vim.keymap.set("n", "<leader>sd", "<cmd>Pick diagnostic scope='current'<cr>", { desc = "Search dignostic" })

	vim.keymap.set("n", "<leader>spf", "<cmd>Pick rg_files cwd=vim.g.mini_deps<cr>", { desc = "Search Package files" })

	vim.keymap.set("n", "<leader>spg", "<cmd>Pick rg_live cwd=vim.g.mini_deps<cr>", { desc = "Search Package grep" })

	vim.keymap.set("n", "<leader>sw", "<cmd>Pick grep_word<cr>", { desc = "Search word" })
	vim.keymap.set("n", "<leader>s/", "<cmd>Pick buf_lines scope='current'<cr>", { desc = "Search /lines" })

	vim.ui.select = MiniPick.ui_select
end)
