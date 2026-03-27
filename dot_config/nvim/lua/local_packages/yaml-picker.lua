local Schemer = {}

---@class SchemerYamlSchema
---@field name string?
---@field uri string
---@field matcher string | table<number,string> | (fun(bufnr: number): boolean?) | nil

---@type SchemerYamlSchema[]
Schemer.user_schemas = {}

---@type SchemerYamlSchema[]
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
---@param schema SchemerYamlSchema
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

---@param bufnr number
Schemer.discover = function(bufnr)
	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]
	local cwd = client.root_dir or vim.uv.cwd() or ""
	for _, schema in ipairs(Schemer.user_schemas) do
		if H.match_schema(bufnr, schema, cwd) then
			return Schemer.set_schema(schema, bufnr)
		end
	end
end

---@param schema SchemerYamlSchema
---@param bufnr number
Schemer.set_schema = function(schema, bufnr)
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
	if #Schemer.store_schemas ~= 0 then return end
	local client = vim.lsp.get_clients({ name = "yamlls" })[1]
	if not client then
		return vim.notify("Schemer: No `yamlls` client attached", vim.log.levels.ERROR)
	end

	local res, err = client:request_sync("yaml/get/all/jsonSchemas", {}, 1000, 0)

	if res and res.result then
		vim.notify("Schemer: populated store schemas", vim.log.levels.DEBUG)
		Schemer.store_schemas = vim.tbl_filter(function(scheme)
			return not vim.tbl_contains(
				Schemer.user_schemas,
				function(v) return v.uri == scheme.uri end,
				{ predicate = true }
			)
		end, res.result)
	elseif res and res.err then
		vim.notify("Schemer: " .. vim.inspect(res.err), vim.log.levels.ERROR)
	else
		vim.notify("Schemer: " .. vim.inspect(err), vim.log.levels.ERROR)
	end
end

---@class SetupConfig
---@field schemas SchemerYamlSchema[]

---@param opts SetupConfig
Schemer.setup = function(opts)
	_G.Schemer = Schemer

	vim.validate("schemas", opts.schemas, function(s) return vim.islist(s) end, 'opts.schemas is list')

	vim.lsp.config("yamlls", {
		handlers = {
			["yaml/schema/store/initialized"] = function(_, _, params, _)
				Schemer.populate_store_schemas()
			end,
		},
		---@param client vim.lsp.Client
		on_init = function(client)
			client.capabilities.workspace.didChangeConfiguration.dynamicRegistration = true
			client:notify("yaml/supportSchemaSelection", { {} })
		end,
		on_attach = function()
			local bufnr = vim.api.nvim_get_current_buf()
			if not vim.b.schemer_yaml_schema then
				Schemer.discover(bufnr)
			end
		end,
	})
	vim.list_extend(Schemer.user_schemas, opts.schemas)

	vim.api.nvim_create_user_command("Schemer", function()
		if vim.bo.filetype ~= "yaml" then
			return vim.notify("Schemer: Not a yaml file", vim.log.levels.ERROR)
		end
		local schemas = {}
		vim.list_extend(schemas, Schemer.store_schemas)
		vim.list_extend(schemas, Schemer.user_schemas)

		if #schemas == 0 then
			return vim.notify("Schemer: No schemas available", vim.log.levels.INFO)
		end

		vim.ui.select(
			schemas,
			{ format_item = function(schema) return schema.name or schema.uri end, prompt = "Select YAML Schema" },
			function(schema)
				if not schema then return end
				Schemer.set_schema(schema, 0)
			end
		)
	end, {})
end

return Schemer
