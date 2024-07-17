-- Which key
require("which-key").setup {
	preset = "helix",
	delay = function(_) -- ctx.plugin
		return 0
	end,
	spec = {
		{ "<leader>l", group = "[L]SP" },
		{ "<leader>s", group = "[S]earch" },
		{ "<leader>w", group = "[W]orkspace" },
	},
}

-- Eyeliner
require("eyeliner").setup {
	highlight_on_key = true,
	dim = true,
}
vim.api.nvim_set_hl(0, "EyelinerPrimary", { link = "@keyword.return" })
vim.api.nvim_set_hl(0, "EyelinerSecondary", { link = "@attribute" })

-- Git gutter
require("gitsigns").setup {
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "_" },
		topdelete = { text = "‾" },
		changedelete = { text = "~" },
	},
}

-- Bufferline+tabline
require("lualine").setup {
	options = {
		icons_enabled = false,
		theme = "auto",
		component_separators = "|",
		section_separators = "",
	},
	sections = {
		lualine_c = {
			{ "filename", path = 1 }
		},
	},
	tabline = {
		lualine_a = {
			{ "buffers", use_mode_colors = true }
		},
	},
}

-- Toggleterm
require("toggleterm").setup {
	direction = "float",
	open_mapping = [[<c-\>]],
}
local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

local function lazygit_toggle()
	lazygit:toggle()
end

vim.api.nvim_create_user_command("Lazygit", lazygit_toggle, { desc = "Lazygit ui" })
vim.keymap.set("n", "<leader>g", lazygit_toggle, { desc = "[G]it" })

-- Bunch of stuff
require("ibl").setup {}
require("Comment").setup {}
require("nvim-surround").setup {}

-- return {
-- 	{
-- 		"neanias/everforest-nvim",
-- 		priority = 1000,
-- 		config = function()
-- 			local ef = require("everforest")
-- 			ef.setup({
-- 				background = "hard",
-- 				disable_italic_comments = true,
-- 			})
-- 			ef.load()
-- 		end,
-- 	},
--
-- 	{
-- 		"rose-pine/neovim",
-- 		priority = 1000,
-- 		name = "rose-pine",
-- 		config = function()
-- 			require("rose-pine").setup({
-- 				styles = {
-- 					italic = false,
-- 				},
-- 			})
-- 			-- vim.cmd.colorscheme("rose-pine")
-- 		end,
-- 	},
--
-- 	{
-- 		"ellisonleao/gruvbox.nvim",
-- 		priority = 1000, -- Ensure it loads first
-- 		config = function()
-- 			require("gruvbox").setup({
-- 				undercurl = false,
-- 				underline = false,
-- 				bold = true,
-- 				italic = {
-- 					strings = false,
-- 					emphasis = true,
-- 					comments = false,
-- 					operators = false,
-- 					folds = false,
-- 				},
-- 			})
-- 			--	vim.cmd.colorscheme("gruvbox")
-- 		end,
-- 	},
--
-- 	-- {
-- 	-- 	"stevearc/oil.nvim",
-- 	-- 	config = function()
-- 	-- 		require("oil").setup({})
-- 	-- 		vim.api.nvim_create_user_command("Ex", function(_)
-- 	-- 			vim.cmd.Oil()
-- 	-- 		end, { desc = "Open oil explorer" })
-- 	-- 	end,
-- 	-- },
--
--
-- 	"tpope/vim-sleuth",
-- }
