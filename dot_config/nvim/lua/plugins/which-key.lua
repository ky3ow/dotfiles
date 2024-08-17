local function setup_buffers()
	local wk = require("which-key")

	local lualine_buffers = require("lualine.components.buffers")
	local function create_goto_keymap(number)
		return {
			"<leader>" .. number,
			function()
				lualine_buffers.buffer_jump(number, "!")
			end,
			desc = function()
				local actual_bufnr = lualine_buffers.bufpos2nr[number]
				local bufname = actual_bufnr and vim.api.nvim_buf_get_name(actual_bufnr) or "-"
				-- :h filename-modifiers
				-- full path, reduced relative to current directory
				local name = vim.fn.fnamemodify(bufname, ":p:.")
				-- name = vim.fn.pathshorten(name)
				return "Go to " .. name
			end,
		}
	end

	for i = 1, 9 do
		wk.add(create_goto_keymap(i))
	end
end

local function setup_visual_at()
	local wk = require("which-key")
	vim.keymap.del("x", "@") -- delete default mapping
	wk.add({
		"@",
		group = "M[@]cro over selection",
		mode = { "x", "n" },
		expand = function()
			local registers = require("which-key.plugins.registers").expand()

			local specs = {} --- @type wk.Spec[]
			for _, reg in ipairs(registers) do
				local bytes = string.byte(reg.key)
				if bytes >= 97 and bytes <= 122 then -- ascii lowercase
					table.insert(specs, {
						reg.key,
						function()
							local cmd =
								vim.api.nvim_replace_termcodes(":normal @" .. reg.key .. "<CR>", true, true, true)
							vim.api.nvim_feedkeys(cmd, "n", false)
						end,
						desc = reg.value,
					})
				end
			end
			return specs
		end,
	})
end

return {
	{
		"folke/which-key.nvim",
		config = function()
			local wk = require("which-key")
			wk.setup({
				preset = "helix",
				delay = function(_) -- ctx.plugin
					return 0
				end,
				spec = {
					{ "<leader>l", group = "[L]SP" },
					{ "<leader>s", group = "[S]earch" },
					{ "<leader>w", group = "[W]orkspace" },
				},
			})

			setup_buffers()
			setup_visual_at()
		end,
	},
}
