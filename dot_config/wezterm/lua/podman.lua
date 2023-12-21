local wezterm = require('wezterm')
local act = wezterm.action
function podman_list()
	local _,container_str,_ = wezterm.run_child_process { "podman", "container", "ls", "--format", "'{{.ID}}:{{.Names}}'" }
	local containers = {}
	for i, n in container_str:gmatch("(%w+):(%w+)") do
		table.insert(containers, { id=i, name=n })
	end
	return containers
end

function make_fixup(id)
	return function(cmd)
		cmd.args = cmd.args or {}
		local wrapped = { 'distrobox', 'enter', id }

		 for _, arg in ipairs(cmd.args) do
		 	table.insert(wrapped, arg)
		 end

		cmd.args = wrapped
		return cmd
	end
end

return {
	setup = function(M)
		for _,container in pairs(podman_list()) do
			table.insert(
				M.exec_domains,
				wezterm.exec_domain(
					container.name,
					make_fixup(container.id),
					container.name
				)
			)
		end
	end
}
