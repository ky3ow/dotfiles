function _G.CHOOSE()
	local M = {}

	---@param bufnr integer
	---@param method string
	M.request_sync = function(bufnr, method)
		local client = vim.lsp.get_clients({ name = "yamlls", bufnr = bufnr })[1]

		if client then
			local response, error = client:request_sync(
				method,
				{ vim.uri_from_bufnr(bufnr) },
				1000,
				bufnr)

			return response
		end
	end

	M.get_all_jsonschemas = function(bufnr)
		return M.request_sync(bufnr, "yaml/get/all/jsonSchemas")
	end

	-- get schema used for {bufnr} from the yamlls attached to it
	---@param bufnr number
	M.get_jsonschema = function(bufnr)
		return M.request_sync(bufnr, "yaml/get/jsonSchema")
	end

	_G.yamlschemas = M.get_all_jsonschemas(0).result

	local schema = _G.yamlschemas[5]

	local client = vim.lsp.get_clients({ name = "yamlls", bufnr = 0 })[1]
	local bufuri = vim.uri_from_bufnr(0)

	-- local overrides = {
	-- 	yaml = {
	-- 		schemas = {
	-- 			["https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.22.4-standalone-strict/all.json"] =
	-- 			bufuri
	-- 		}
	-- 	}
	-- }

	local filtered_schemas = vim.tbl_filter(function(fileuri)
		return bufuri ~= fileuri
	end, client.settings.yaml.schemas)

	client.settings.yaml.schemas = filtered_schemas

	local overrides = {
	 yaml = {
	   schemas = {
	     [schema.uri] = bufuri
	   }
	 }
	}

	client.settings = vim.tbl_deep_extend("force", client.settings, overrides)
	vim.notify(vim.inspect(client.settings))
	client:notify("workspace/didChangeConfiguration", { settings = client.settings })
	vim.notify(vim.inspect(M.get_jsonschema(0)))

	--[[
	----- Callback to be passed to vim.ui.select to display a single schema item
	--- @param schema table: Schema
	local display_schema_item = function(schema)
		return schema.name or schema.uri
	end

	--- Callback to be passed to vim.ui.select that changes the active yaml schema
	--- @param schema table: Chosen schema
	local select_schema = function(schema)
		if not schema then
			return
		end
		local selected_schema = { name = schema.name, uri = schema.uri }
		require("yaml-companion.context").schema(0, selected_schema)
	end

	M.open_ui_select = function()
		local schemas = require("yaml-companion.schema").all()

		-- Don't open selection if there are no available schemas
		if #schemas == 0 then
			return
		end

		vim.ui.select(
			schemas,
			{ format_item = display_schema_item, prompt = "Select YAML Schema" },
			select_schema
		)
	end
	--]]
end
