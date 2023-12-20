local M = {}

local palette = {
    base = '#2d353b',
    overlay = '#3d484d',
    muted = '#232a2e',
    text = '#d3c6aa',
    red = '#e67e80',
    yellow = '#dbbc7f',
    orange = '#e69875',
    green = '#a7c080',
    cyan = '#7fbbb3',
    violet = '#d699b6',
    highlight_high = '#dbbc7f',
}

local active_tab = {
    bg_color = palette.overlay,
    fg_color = palette.cyan,
}

local inactive_tab = {
    bg_color = palette.muted,
    fg_color = palette.text,
}

function M.colors()
    return {
        foreground = palette.text,
        background = palette.base,
        cursor_bg = palette.highlight_high,
        cursor_border = palette.highlight_high,
        cursor_fg = palette.muted,
        selection_bg = "#543a48",
        selection_fg = palette.text,

        ansi = {
            palette.overlay,
            palette.red,
            palette.green,
            palette.yellow,
            palette.cyan,
            palette.violet,
            palette.orange,
            palette.text,
        },

        brights = {
            palette.muted,
            palette.red,
            palette.green,
            palette.yellow,
            palette.cyan,
            palette.violet,
            palette.orange,
            palette.text,
        },

        tab_bar = {
            background = palette.base,
            active_tab = active_tab,
            inactive_tab = inactive_tab,
            inactive_tab_hover = active_tab,
            new_tab = inactive_tab,
            new_tab_hover = active_tab,
            inactive_tab_edge = palette.muted, -- (Fancy tab bar only)
        },
    }
end

function M.window_frame() -- (Fancy tab bar only)
    return {
        active_titlebar_bg = palette.base,
        inactive_titlebar_bg = palette.base,
    }
end

return M

