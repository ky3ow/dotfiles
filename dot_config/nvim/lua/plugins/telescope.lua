local function find_git_root()
	local current_file = vim.api.nvim_buf_get_name(0)
	local current_dir
	local cwd = vim.fn.getcwd()
	if current_file == '' then
		current_dir = cwd
	else
		current_dir = vim.fn.fnamemodify(current_file, ':h')
	end
	local git_root = vim.fn.systemlist('git -C ' .. vim.fn.escape(current_dir, ' ') .. ' rev-parse --show-toplevel')
		[1]
	if vim.v.shell_error ~= 0 then
		print 'Not a git repository. Searching on current working directory'
		return cwd
	end
	return git_root
end

local function live_grep_git_root()
	local git_root = find_git_root()
	if git_root then
		require('telescope.builtin').live_grep {
			search_dirs = { git_root },
		}
	end
end

local function buffer_grep()
	require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
		winblend = 10,
		previewer = false,
	})
end

local function all_buffers_grep()
	require('telescope.builtin').live_grep {
		grep_open_files = true,
		prompt_title = 'Live Grep in Open Files',
	}
end

local function setup()
	local actions = require("telescope.actions")
	local builtins = require('telescope.builtin')

	require('telescope').setup {
		defaults = {
			mappings = {
				i = {
					['<esc>'] = actions.close,
					['<C-u>'] = false,
					['<C-f>'] = actions.preview_scrolling_down,
					['<C-b>'] = actions.preview_scrolling_up,
				}
			}
		}
	}
	pcall(require('telescope').load_extension, 'fzf')

	vim.keymap.set('n', '<leader>?', builtins.oldfiles, { desc = '[?] Find recently opened files' })
	vim.keymap.set('n', '<leader><space>', builtins.buffers, { desc = '[ ] Find existing buffers' })
	vim.keymap.set('n', '<leader>/', buffer_grep, { desc = '[/] Fuzzily search in current buffer' })

	vim.keymap.set('n', '<leader>s/', all_buffers_grep, { desc = '[S]earch [/] in Open Files' })
	vim.keymap.set('n', '<leader>ss', builtins.builtin, { desc = '[S]earch [S]elect Telescope' })
	vim.keymap.set('n', '<leader>gf', builtins.git_files, { desc = 'Search [G]it [F]iles' })
	vim.keymap.set('n', '<leader>sf', builtins.find_files, { desc = '[S]earch [F]iles' })
	vim.keymap.set('n', '<leader>sh', builtins.help_tags, { desc = '[S]earch [H]elp' })
	vim.keymap.set('n', '<leader>sw', builtins.grep_string, { desc = '[S]earch current [W]ord' })
	vim.keymap.set('n', '<leader>sg', builtins.live_grep, { desc = '[S]earch by [G]rep' })
	vim.keymap.set('n', '<leader>sG', ':LiveGrepGitRoot<cr>', { desc = '[S]earch by [G]rep on Git Root' })
	vim.keymap.set('n', '<leader>sd', builtins.diagnostics, { desc = '[S]earch [D]iagnostics' })
	vim.keymap.set('n', '<leader>sr', builtins.resume, { desc = '[S]earch [R]esume' })
	vim.api.nvim_create_user_command('LiveGrepGitRoot', live_grep_git_root, {})
end


return {
	{
		'nvim-telescope/telescope.nvim',
		config = setup,
		branch = '0.1.x',
		dependencies = {
			'nvim-lua/plenary.nvim',
			{
				'nvim-telescope/telescope-fzf-native.nvim',
				build = 'make', -- make is installed on system
				cond = function()
					return vim.fn.executable 'make' == 1
				end,
			},
		},
	},
}
