vim.keymap.set("n", "<localleader>s", "<cmd>Schemer<cr>", { desc = "Open schema selector", buffer = 0 })

if vim.g.schemer_initialized then
	return
end

vim.g.schemer_initialized = true

require "packages/yaml-picker".setup {
	schemas = {
		{
			name = "Kubernetes",
			uri = "kubernetes",
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
}
