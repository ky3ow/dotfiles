MiniDeps.later(function()
	local ai = require("mini.ai")
	local ts = ai.gen_spec.treesitter

	ai.setup {
		mappings = {
			goto_left = "[m",
			goto_right = "]m",
		},
		custom_textobjects = {
			f = ts { a = "@function.outer", i = "@function.inner", },
			a = ts { a = "@parameter.outer", i = "@parameter.inner", },
			o = ts { a = "@block.outer", i = "@block.inner", },
			c = ts { a = "@class.outer", i = "@class.inner", },
			C = ts { a = "@call.outer", i = "@call.inner", },
			m = ts { i = { "@block.inner", "@block.outer" }, a = { "@block.inner", "@block.outer" } },
		}
	}

	MiniDeps.add {
		source = "nvim-treesitter/nvim-treesitter",
		checkout = "master",
		monitor = "main",
		hooks = {
			post_checkout = function()
				vim.cmd("TSUpdate")
			end
		}
	}

	MiniDeps.add "nvim-treesitter/nvim-treesitter-textobjects"
	MiniDeps.add "nvim-treesitter/nvim-treesitter-context"

	require("nvim-treesitter.configs").setup {
		ensure_installed = { "lua", "python", "vimdoc", "vim", "bash", "markdown" },
		auto_install = false,
		sync_install = false,
		ignore_install = {},
		highlight = {
			enable = true,
			disable = function(lang, buf)
				local max_filesize = 1024 * 1024 * 2 -- 2MB
				local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
				if ok and stats and stats.size > max_filesize then
					return true
				end
			end,
		},
		indent = {
			enable = true
		},
	}
end)
