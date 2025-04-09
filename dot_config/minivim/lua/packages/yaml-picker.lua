local Schemer = {}
_G.Schemer = Schemer

---@class SchemerUserSchema
---@field name string?
---@field uri string
---@field matcher string | table | (fun(bufnr: number): boolean) | nil

---@type SchemerUserSchema[]
Schemer.user_schemas = {
	{
		name = "Kubernetes",
		-- uri = "kubernetes",
		uri = "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.32.1-standalone-strict/all.json",

		matcher = function(bufnr)
			local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
			for _, line in ipairs(lines) do
				for _, resource in ipairs({ "ConfigMap" }) do
					if vim.regex("^kind: " .. resource .. "$"):match_str(line) then
						return true
					end
				end
			end
			return false
		end,
		description = "Kubernetes schema v1.32.1"
	},
	{
		name = "Webhooks",
		uri = "https://unpkg.com/@octokit/webhooks-schemas@7.6.1/schema.json",
		matcher = { "/abc.yaml", "/def.yaml" },
	},
	{
		uri = "https://bonkers.json",
		matcher = "/cde.yaml"
	},
	{
		uri = "https://boop.bonkers.json",
		matcher = "file:///home/v/code/abc.yaml"
	},
	{
		uri = "https://aaaa.what.a.json",
		matcher = "/*.yaml"
	},
	{
		uri = "https://what.a.json",
		matcher = { "/*.yaml", "/*.yml" }
	},
	{
		uri = "https://bbb.what.a.json",
		matcher = "/**/*.yaml"
	},
}

Schemer.store_schemas = {}

local H = {}

---@param filename string
---@param path string
---@param cwd string
H.match_filepath = function(filename, path, cwd)
	local _, absolute_prefix = vim.regex("^file://"):match_str(path)
	path = absolute_prefix and path:sub(absolute_prefix + 1, #path) or vim.fs.joinpath(cwd, path)
	local pattern = vim.glob.to_lpeg(path)
	return pattern:match(filename) ~= nil
end

---@param bufnr number
---@param schema SchemerUserSchema
---@param cwd string
H.match_schema = function(bufnr, schema, cwd)
	local matcher = schema.matcher
	if type(matcher) == "function" then
		return matcher(bufnr)
	end
	local filename = vim.api.nvim_buf_get_name(bufnr)

	if type(matcher) == "string" then
		return H.match_filepath(filename, matcher, cwd)
	end

	if type(matcher) == "table" then
		for _, match in ipairs(matcher) do
			if H.match_filepath(filename, match, cwd) then return true end
		end
	end
end

Schemer.discover = function(bufnr)
	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]
	local cwd = client.root_dir or vim.uv.cwd() or ""
	for _, schema in ipairs(Schemer.user_schemas) do
		if H.match_schema(bufnr, schema, cwd) then
			Schemer.set_schema(schema, bufnr)
			return
		end
	end
end

Schemer.set_schema = function(schema, bufnr)
	if not schema then
		return
	end

	local bufuri = vim.uri_from_bufnr(bufnr)
	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]

	local schemas = client.settings.yaml.schemas

	for schema_uri, files in pairs(schemas) do
		if type(schemas[schema_uri]) == "string" then
			files = { files }
			schemas[schema_uri] = files
		end

		for index, file in ipairs(files) do
			if file == bufuri then
				table.remove(files, index)
			end
		end
	end

	if not schemas[schema.uri] then schemas[schema.uri] = {} end
	table.insert(schemas[schema.uri], bufuri)

	client:notify("workspace/didChangeConfiguration", { settings = client.settings })
	vim.b[bufnr].schemer_yaml_schema = schema
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
		function(schema) Schemer.set_schema(schema, 0) end
	)
end, {})
