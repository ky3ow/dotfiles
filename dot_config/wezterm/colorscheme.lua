local colors = require("wezterm").color.get_builtin_schemes()['Ef-Elea-Dark']

colors.ansi = {
    "#303332",
    "#ff656a",
    "#7fc87f",
    "#cac85f",
    "#57aff6",
    "#f59acf",
    "#6fcfd2",
    "#eaf2ef",
}

colors.brights = {
    "#5e6160",
    "#fa7f88",
    "#50cf89",
    "#cfb27f",
    "#62cfef",
    "#cfaaff",
    "#60d5c2",
    "#969faf",
}

colors.background = '#222524'
colors.foreground = '#eaf2ef'
colors.cursor_fg = colors.background
colors.cursor_bg = '#ef7fa8'
colors.selection_bg = '#543040'
colors.selection_fg = '#eaf2ef'
colors.indexed = {}

colors.tab_bar = {
  background = colors.background
}

Config.color_schemes = {
  Elea = colors
}
Config.color_scheme = 'Elea'

Config.command_palette_bg_color = colors.background

local function basename(s)
    return string.gsub(s, '(.*[/\\])(.*)', '%2')
end

local function tab_title(tab_info)
  local title = tab_info.tab_title
  if title and #title > 0 then
    return title
  end
  local pane = tab_info.active_pane
  return basename(pane.foreground_process_name)
end

wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    local title = tab_title(tab)
    local index = tab.tab_index + 1

    if tab.is_active then
      return {
        { Background = { Color = '#222524' } },
        { Foreground = { Color = '#7fc87f'} },
        { Text = string.format(" %d: %s ", index, title) },
      }
    end

    return {
      { Background = { Color = '#222524' }},
      { Text = string.format(" %d: %s ", index, title) },
    }
  end
)
