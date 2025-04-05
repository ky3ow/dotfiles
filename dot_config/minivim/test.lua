local Schemer = {}

Schemer.discovery_schemas = {
	["kubernetes"] = function(bufnr)
		local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
		for _, line in ipairs(lines) do
			for _, resource in ipairs({ "ConfigMap" }) do
				if vim.regex("^kind: " .. resource .. "$"):match_str(line) then
					return true
				end
			end
		end
	end
}

Schemer.discover = function(bufnr)
	for schema_uri, matcher in pairs(Schemer.discovery_schemas) do
		if matcher(bufnr) then
			Schemer.set_schema({ uri = schema_uri }, bufnr)
		end
	end
end

Schemer.set_schema = function(schema, bufnr)
	if not schema then
		return
	end

	local bufuri = vim.uri_from_bufnr(bufnr)
	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]

	local overrides = {
		yaml = {
			schemas = {
				[schema.uri] = bufuri
			}
		}
	}

	local filtered_schemas = vim.tbl_filter(function(fileuri)
		return bufuri ~= fileuri
	end, client.settings.yaml.schemas)

	client.settings.yaml.schemas = filtered_schemas
	client.settings = vim.tbl_deep_extend("force", client.settings, overrides)
	client:notify("workspace/didChangeConfiguration", { settings = client.settings })

	vim.notify(string.format("Changed workspace configuration %s to use %s", bufuri, schema.uri))
	vim.b[bufnr].yaml_schema = schema
end

local H = {}

H.get_all = function()
	local bufnr = 0

	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]
	if not client then
		vim.notify("Schemer: No `yamlls` client attached")
		return
	end

	local res, err = client:request_sync("yaml/get/all/jsonSchemas", {}, 1000, bufnr)

	if res and res.result then return res.result, nil end
	if res and res.err then return nil, res.err end
	return nil, err
end

vim.api.nvim_create_user_command("Schemer", function(opts)
	local bufnr = 0

	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]
	if not client then
		vim.notify("Schemer: No `yamlls` client attached")
		return
	end

	-- local bufuri = vim.uri_from_bufnr(bufnr)
	-- local res, err = client:request_sync("yaml/get/all/jsonSchemas", { bufuri }, 1000, bufnr)
	local res, err = client:request_sync("yaml/get/all/jsonSchemas", {}, 1000, bufnr)

	if not res then
		vim.notify(vim.inspect(err))
		return
	elseif res and res.err then
		vim.notify(vim.inspect(err))
		return
	end

	local schemas = res.result

	if #schemas == 0 then
		vim.notify("No schemas available")
		return
	end

	vim.ui.select(
		schemas,
		{ format_item = function(schema) return schema.name or schema.uri end, prompt = "Select YAML Schema" },
		function(schema)
			Schemer.set_schema(schema, bufnr)
		end
	)
end, {})
