local wezterm = require('wezterm')
local act = wezterm.action
return {
	setup = function(M)
		M.keys = {
			{
				key = "j",
				mods = "CTRL|SHIFT",
				action = act.ActivatePaneDirection "Down",
			},
			{
				key = "k",
				mods = "CTRL|SHIFT",
				action = act.ActivatePaneDirection "Up",
			},
			{
				key = "h",
				mods = "CTRL|SHIFT",
				action = act.ActivatePaneDirection "Left",
			},
			{
				key = "l",
				mods = "CTRL|SHIFT",
				action = act.ActivatePaneDirection "Right",
			},
			{
				key = "s",
				mods = "CTRL|SHIFT",
				action = act.PaneSelect { alphabet = "123456789" },
			},
			{
				key = "y",
				mods = "CTRL|SHIFT",
				action = act.QuickSelect,
			},
			{
				key = "l",
				mods = "CTRL|SHIFT",
				action = act.ShowLauncher,
			},
		}
		M.key_map_preference = "Physical"
	end
}
