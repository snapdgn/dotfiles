-- Pull in the wezterm API
local wezterm = require "wezterm"

-- This will hold the configuration
local config = wezterm.config_builder()

-- Disable tab bar
config.enable_tab_bar = false
--config.use_fancy_tab_bar = false
--config.hide_tab_bar_if_only_one_tab = true

config.initial_cols = 240
config.initial_rows = 56

-- Opacity + blur (macOS)
config.window_background_opacity = 1.0
config.macos_window_background_blur = 20

-- Font settings
config.font_size = 16
config.font = wezterm.font_with_fallback({
    --{ family = "Victor Mono", weight = "Medium" },
    --"FiraCode Nerd Font",
    --{ family = "Zed Plex Mono", weight = 450 },
    "Iosevka Nerd Font",
    "BlexMono Nerd Font Mono",
    "Lilex Nerd Font",
    "Inconsolata Nerd Font",
    "PragmataPro",
    "Pragmasevka",
    "Iosevka SS15",
    "PlemolJP",
    "NotoSans Nerd Font",
    "Fira Code",
    "Fira Mono",
    "DengXian",
})

---- Match Zed's line spacing: Zed uses font_size * 1.618 ("comfortable"),
---- wezterm multiplies the font's natural 1.3em line height -> 1.618/1.3
--config.line_height = 1.245

---- Zed rasterizes via CoreText, which does no hinting; wezterm's FreeType
---- hints by default, which snaps some stems thin and smears others.
--config.freetype_load_flags = "NO_HINTING"

config.window_decorations = "RESIZE"
config.automatically_reload_config = true


-- fix option+left/right on macos
config.keys = {
    -- Option + arrows
    { key = "LeftArrow",  mods = "OPT", action = wezterm.action { SendString = "\x1bb" } },
    { key = "RightArrow", mods = "OPT", action = wezterm.action { SendString = "\x1bf" } },

    -- Fullscreen toggle
    {
        key = "f",
        mods = "META|SHIFT",
        action = wezterm.action.ToggleFullScreen,
    },
}

-- IST offset in seconds (UTC + 5:30)
local IST_OFFSET = 5.5 * 60 * 60

local function is_daytime_ist()
    local hour = tonumber(os.date("!%H", os.time() + IST_OFFSET))
    return hour >= 7 and hour < 19
end

-- Dynamic theme based on IST time
if is_daytime_ist() then
    --config.color_scheme = 'Everforest Light Medium (Gogh)'
    --config.color_scheme = 'Everforest Dark Hard (Gogh)'
    --config.color_scheme = 'Nord Light (Gogh)'
    --config.color_scheme = 'Nord (Gogh)'
    --config.color_scheme = 'Rosé Pine Moon (base16)'
    --config.color_scheme = 'One Dark (Gogh)'
    config.color_scheme = 'Everforest Dark (Gogh)'
    --config.color_scheme = 'One Dark (Gogh)'
    --config.color_scheme = 'Rosé Pine Dawn (base16)'
    --config.color_scheme = 'rose-pine'
    --config.color_scheme = 'Everforest Light (Gogh)'
    --config.color_scheme = 'Gruvbox Material (Gogh)'
    --config.color_scheme = 'Solarized Dark Higher Contrast'
    --config.color_scheme = "tokyonight_moon"
    --config.color_scheme = 'Google (light) (terminal.sexy)'
    --config.color_scheme = 'Solarized (dark) (terminal.sexy)'
    --config.color_scheme = 'nordfox'
    --config.color_scheme = 'nord'
else
    --config.color_scheme = 'nord'
    --config.color_scheme = 'Nord (Gogh)'
    --config.color_scheme = 'Nord Light (Gogh)'
    --config.color_scheme = 'Github Light (Gogh)'
    --config.color_scheme = 'Solarized (dark) (terminal.sexy)'
    --config.color_scheme = 'Gruvbox Material (Gogh)'
    --config.color_scheme = 'Gruvbox light, medium (base16)'
    --config.color_scheme = 'Everforest Dark Hard (Gogh)'
    --config.color_scheme = 'Everforest Dark (Gogh)'
    --config.color_scheme = 'rose-pine'
    --config.color_scheme = 'Rosé Pine Moon (base16)'
    config.color_scheme = 'One Dark (Gogh)'
    --config.color_scheme = 'Everforest Light Medium (Gogh)'
    --config.color_scheme = "tokyonight_moon"
    --config.color_scheme = 'nordfox'
end

--config.color_scheme = 'Everforest Light (Gogh)'

-- Fullscreen on startup
local mux = wezterm.mux
wezterm.on("gui-startup", function()
    local window = mux.spawn_window({})
    window:gui_window():toggle_fullscreen()
end)

-- key collision
--config.keys = {
--{
--key = 'f',
--mods = 'META',
--action = wezterm.action.Search({ CaseInSensitiveString = '' })
--}
--}

return config
