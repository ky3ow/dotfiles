local config = { keys = {}, unix_domains = {}, exec_domains = {} }
local wezterm = require('wezterm')
local act = wezterm.action

-- Theme --
require('lua/theme').setup(config)

-- Keys --
require('lua/keyboard').setup(config)

-- Distrobox --
require('lua/podman').setup(config)

-- WSL --
if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    table.insert(config.unix_domains, { name = 'wsl', serve_command = { 'wsl', 'wezterm-mux-server', '--daemonize' } })
end

-- ShitBook --
if wezterm.target_triple == "x86_64-apple-darwin" then
    require('lua/mac').setup(config)
end

-- Linux --
if wezterm.target_triple == "x86_64-unknown-linux-gnu" then
    require('lua/linux').setup(config)
end

return config
