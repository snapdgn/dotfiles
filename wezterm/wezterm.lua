-- Pull in the wezterm API
local wezterm = require "wezterm"

-- This will hold the configuration
local config = wezterm.config_builder()

-- Disable tab bar
config.enable_tab_bar = false

config.initial_cols = 120
config.initial_rows = 28

-- Opacity + blur (macOS)
config.window_background_opacity = 0.5
config.macos_window_background_blur = 20

-- Font settings
config.font_size = 18
config.font = wezterm.font_with_fallback({
  "Iosevka SS15",
  "Zed Plex Mono",
  "Fira Code",
  "DengXian",
})

config.window_decorations = "RESIZE"
config.automatically_reload_config = true

-- IST offset in seconds (UTC + 5:30)
local IST_OFFSET = 5.5 * 60 * 60

local function is_daytime_ist()
  local now_utc = os.time()
  local ist_time = now_utc + IST_OFFSET
  local ist_date = os.date("*t", ist_time)
  local hour = ist_date.hour
  return hour >= 7 and hour < 19
end

-- Dynamic theme based on IST time
if is_daytime_ist() then
  config.color_scheme = "tokyonight_moon"
else
  config.color_scheme = "tokyonight_moon"
end

-- Fullscreen on startup
local mux = wezterm.mux
wezterm.on("gui-startup", function()
  local window = mux.spawn_window({})
  window:gui_window():toggle_fullscreen()
end)

-- Keybinding to toggle fullscreen
config.keys = {
  {
    key = "f",
    mods = "META|SHIFT",
    action = wezterm.action.ToggleFullScreen,
  },
}

return config
