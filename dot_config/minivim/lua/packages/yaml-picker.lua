MiniDeps.add "someone-stole-my-name/yaml-companion.nvim"

local cfg = require("yaml-companion").setup({
	schemas = {},
  -- lspconfig = {
  --   cmd = {"yaml-language-server"}
  -- },
})
require("lspconfig")["yamlls"].setup(cfg)

vim.api.nvim_create_user_command("Schema", function(_)
	require"yaml-companion".open_ui_select()
end, { desc = "Select yaml schema" })
