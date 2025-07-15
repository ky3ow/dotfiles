MiniDeps.now(function()
	if vim.fn.executable "tree-sitter" == 0 then
		vim.cmd "echo 'Installing tree-sitter cli' | redraw"

		local ts_cli = "https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.8/tree-sitter-linux-x64.gz"
		local ts_tempdir = vim.fn.expand(vim.fn.stdpath "cache" .. "/treesitter")
		local ts_archivename = vim.fn.expand(ts_tempdir .. "/tree-sitter.gz")
		local ts_downloaded_executable = vim.fn.expand(ts_tempdir .. "/tree-sitter")
		local ts_targetdir = vim.g.path_binary
		local ts_executable = vim.fn.expand(ts_targetdir .. "/tree-sitter")
		vim.system({ "mkdir", "-p", ts_tempdir }):wait()
		vim.system({ "mkdir", "-p", ts_targetdir }):wait()
		vim.system({ "curl", "-L", "-o", ts_archivename, ts_cli }):wait()
		vim.system({ "gzip", "-d", ts_archivename }, { cwd = ts_tempdir }):wait()
		vim.system({ "chmod", "+x", ts_downloaded_executable }):wait()
		vim.system({ "mv", ts_downloaded_executable, ts_executable }):wait()
	end

	MiniDeps.add {
		source = "nvim-treesitter/nvim-treesitter",
		checkout = "main",
		hooks = {
			post_checkout = function()
				vim.cmd "TSUpdate"
			end,
		},
	}

	require("nvim-treesitter").install { "lua", "python", "vimdoc", "vim", "bash", "markdown" }

	vim.api.nvim_create_autocmd("FileType", {
		group = vim.api.nvim_create_augroup("ky3ow.Treesitter", { clear = true }),
		callback = function(ev)
			local max_filesize = 1024 * 1024 * 2 -- 2MB
			local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(ev.buf))

			if ok and stats and stats.size > max_filesize then
				vim.notify "File too big for tree-sitter"
				return
			end

			if pcall(vim.treesitter.start) then
				vim.wo.foldmethod = "expr"
				vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
				vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
			end
		end,
	})
end)

MiniDeps.later(function()
	local ai = require "mini.ai"
	local ts = ai.gen_spec.treesitter

	ai.setup {
		mappings = {
			goto_left = "[m",
			goto_right = "]m",
		},
		custom_textobjects = {
			f = ts { a = "@function.outer", i = "@function.inner" },
			a = ts { a = "@parameter.outer", i = "@parameter.inner" },
			o = ts { a = "@block.outer", i = "@block.inner" },
			c = ts { a = "@class.outer", i = "@class.inner" },
			C = ts { a = "@call.outer", i = "@call.inner" },
			m = ts { i = { "@block.inner", "@block.outer" }, a = { "@block.inner", "@block.outer" } },
		},
	}

	MiniDeps.add { source = "nvim-treesitter/nvim-treesitter-textobjects", checkout = "main" }
	MiniDeps.add "nvim-treesitter/nvim-treesitter-context"
end)
