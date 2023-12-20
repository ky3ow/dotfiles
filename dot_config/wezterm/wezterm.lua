local config = { keys = {}, unix_domains = {} }
local wezterm = require('wezterm')
local act = wezterm.action
-- Theme --
require('lua/theme').configure(config)

-- Keys --
config.keys = {
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
        action = act.QuickSelect
    }
}

config.key_map_preference = "Physical"

-- WSL --
if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    table.insert(config.unix_domains, { name = 'wsl', serve_command = { 'wsl', 'wezterm-mux-server', '--daemonize' } })
end

-- ShitBook --
if wezterm.target_triple == "x86_64-apple-darwin" then
    require('lua/mac').configure_mac(config)
end

-- Linux --
if wezterm.target_triple == "x86_64-unknown-linux-gnu" then
    require('lua/linux').configure_linux(config)
end

return config
