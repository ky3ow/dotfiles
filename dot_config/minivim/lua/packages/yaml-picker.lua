-- MiniDeps.add "someone-stole-my-name/yaml-companion.nvim"

-- local cfg = require("yaml-companion").setup({
-- 	schemas = {},
  -- lspconfig = {
  --   cmd = {"yaml-language-server"}
  -- },
-- })
-- require("lspconfig")["yamlls"].setup(cfg)

-- vim.api.nvim_create_user_command("Schema", function(_)
-- 	require"yaml-companion".open_ui_select()
-- end, { desc = "Select yaml schema" })

--[[
local YamlBuddy = {}
local H = {}

H.default_config = {
}

H.setup_config = function(config)
	config = vim.tbl_deep_extend('force', vim.deepcopy(H.default_config), config or {})
	return config
end

H.add_hook_after = function(func, new_fn)
  if func then
    return function(...)
      -- TODO which result?
      func(...)
      return new_fn(...)
    end
  else
    return new_fn
  end
end

YamlBuddy.setup = function(config)
end
--]]
