local Schemer = {}
_G.Schemer = Schemer

Schemer.user_schemas = {
	{
		name = "Kubernetes",
		uri =
		"https://raw.githubusercontent.com/yannh/kubernetes-json-schema/refs/heads/master/v1.32.1-standalone-strict/all.json",
		matcher = function(bufnr)
			local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
			for _, line in ipairs(lines) do
				for _, resource in ipairs({ "ConfigMap" }) do
					if vim.regex("^kind: " .. resource .. "$"):match_str(line) then
						return true
					end
				end
			end
		end,
		fromStore = false,
		description = "Kubernetes schema v.1.32.1"
	},
	{
		name = "BBB",
		uri = "https://osmethin.json",
		matcher = function(bufnr)
			return false
		end,
		fromStore = false,
		description = "Bbb"
	},
	{
		uri = "https://unpkg.com/@octokit/webhooks-schemas@7.6.1/schema.json",
		matcher = "/abc.yaml",
	},
	{
		uri = "https://bonkers.json",
		matcher = "/cde.yaml"
	}
}

Schemer.store_schemas = {}

Schemer.discover = function(bufnr)
	for _, schema in ipairs(Schemer.user_schemas) do
		if type(schema.matcher) == "function" then
			if schema.matcher(bufnr) then
				Schemer.set_schema(schema, bufnr)
			end
		end
	end
end

Schemer.set_schema = function(schema, bufnr)
	if not schema then
		return
	end

	local bufuri = vim.uri_from_bufnr(bufnr)
	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]

	---@type table
	local schemas = client.settings.yaml.schemas

	for schema_uri, file in pairs(schemas) do
		if file == bufuri then
			schemas[schema_uri] = nil
		end
	end
	schemas[schema.uri] = bufuri

	client:notify("workspace/didChangeConfiguration", { settings = client.settings })
	vim.notify(string.format("Changed workspace configuration %s to use %s", bufuri, schema.uri))
	vim.b[bufnr].yaml_schema = schema
end

Schemer.populate_store_schemas = function()
	local client = vim.lsp.get_clients({ name = "yamlls" })[1]
	if not client then
		vim.notify("Schemer: No `yamlls` client attached")
		return
	end

	local res, err = client:request_sync("yaml/get/all/jsonSchemas", {}, 1000, 0)

	if res and res.result then
		vim.notify("Schemer: populated store schemas")
		Schemer.store_schemas = vim.tbl_filter(function(scheme)
			return not vim.tbl_contains(
				Schemer.user_schemas,
				function(v) return v.uri == scheme.uri end,
				{ predicate = true }
			)
		end, res.result)
	elseif res and res.err then
		vim.notify(vim.inspect(res.err))
	else
		vim.notify(vim.inspect(err))
	end
end

vim.api.nvim_create_user_command("Schemer", function(opts)
	local bufnr = 0
	local schemas = {}
	vim.list_extend(schemas, Schemer.store_schemas)
	vim.list_extend(schemas, Schemer.user_schemas)

	if #schemas == 0 then
		vim.notify("No schemas available")
		return
	end

	vim.ui.select(
		schemas,
		{ format_item = function(schema) return schema.name or schema.uri end, prompt = "Select YAML Schema" },
		function(schema) Schemer.set_schema(schema, bufnr) end
	)
end, {})

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

--

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

