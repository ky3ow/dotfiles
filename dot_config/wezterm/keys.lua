local leader = {
  key = "s",
  mods = "CTRL",
}

Config.leader = {
  key = leader.key,
  mods = leader.mods,
  timeout_milliseconds = 1000,
}

Config.enable_kitty_keyboard = true

Config.keys = {
  {
    key = '"',
    mods = "LEADER|SHIFT", 
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = "%",
    mods = "LEADER|SHIFT",
    action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" },
  },
  {
    key = leader.key,
    mods = "LEADER|" .. leader.mods,
    action = wezterm.action.SendKey {
      key = leader.key,
      mods = leader.mods,
    }
  },
  {
    key = "h",
    mods = "LEADER",
    action = wezterm.action.ActivatePaneDirection "Left",
  },
  {
    key = "j",
    mods = "LEADER",
    action = wezterm.action.ActivatePaneDirection "Down",
  },
  {
    key = "k",
    mods = "LEADER",
    action = wezterm.action.ActivatePaneDirection "Up",
  },
  {
    key = "l",
    mods = "LEADER",
    action = wezterm.action.ActivatePaneDirection "Right",
  },
 
  {
    key = "n",
    mods = "LEADER",
    name = "next",
    action = wezterm.action.Search {
      CaseSensitiveString = ""
    }
  },

  {
    key = ":",
    mods = "LEADER|SHIFT",
    action = wezterm.action.ShowLauncher
  },
  
  {
    key = "s",
    mods = "LEADER",
    action = wezterm.action.ShowLauncherArgs { flags = "WORKSPACES" }
  },
  
  {
    key = "w",
    mods = "LEADER",
    action = wezterm.action.ShowTabNavigator
  },

  {
    key = "y",
    mods = "LEADER",
    action = wezterm.action_callback(function(win,pane)
      local zones = pane:get_semantic_zones()
      wezterm.log_info(zones)
      for i = #zones, 1, -1 do
        local zone = zones[i]
        if zone.semantic_type == "Output" then
          -- local text = pane:get_text_from_semantic_zone(zone) -- needs fix
          local text = pane:get_text_from_region(zone.start_x, zone.start_y, zone.end_x, zone.end_y + 1)
          wezterm.log_info(zone)
          wezterm.log_info(text)
          win:copy_to_clipboard(text)
          return
        end
      end
    end)
  },
}

