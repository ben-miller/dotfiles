local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.keys = {
  {
    key = 'r',
    mods = 'CMD|SHIFT',
    action = wezterm.action.ReloadConfiguration,
  },
  {
    -- Split pane vertically (wezterm)
    key = 'd',
    mods = 'CMD|OPT',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    -- Split pane vertically (tmux)
    key = 'd',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="%", mods=""}),
      }),
  },
  {
    -- Rebalance panes (tmux)
    key = 'Enter',
    mods = 'CMD|OPT',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="1", mods="OPT"}),
      }),
  },
  {
    -- Split pane horizontally (wezterm)
    key = 'd',
    mods = 'CMD|SHIFT',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    -- Split pane horizontally (tmux)
    key = 'd',
    mods = 'CMD|SHIFT',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="\"", mods=""}),
      }),
  },
  {
    -- Move left (wezterm)
    key = 'h',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivatePaneDirection "Left",
  },
  {
    -- Move left (tmux)
    key = 'h',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="LeftArrow", mods=""}),
      }),
  },
  {
    -- Move right (wezterm)
    key = 'l',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivatePaneDirection "Right",
  },
  {
    -- Move right (tmux)
    key = 'l',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="RightArrow", mods=""}),
      }),
  },
  {
    -- Move up (wezterm)
    key = 'k',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivatePaneDirection "Up",
  },
  {
    -- Move up (tmux)
    key = 'k',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="UpArrow", mods=""}),
      }),
  },
  {
    -- Move down (wezterm)
    key = 'j',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivatePaneDirection "Down",
  },
  {
    -- Move down (tmux)
    key = 'j',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="DownArrow", mods=""}),
      }),
  },
  {
    key = 'Enter',
    mods = 'CMD',
    action = wezterm.action.Multiple {
      wezterm.action.SendString '\nclear\n',
      wezterm.action.SendString 'tmux clear-history\n',
    },
  },
  {
    -- Toggle current pane zoom (wezterm)
    key = 'Enter',
    mods = 'CMD|OPT|SHIFT',
    action = wezterm.action.TogglePaneZoomState
  },
  {
    -- Toggle current pane zoom (tmux)
    key = 'Enter',
    mods = 'CMD|SHIFT',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="z", mods=""}),
      }),
  },
  {
    -- Move to left tab (wezterm)
    key = 'LeftArrow',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivateTabRelativeNoWrap(-1)
  },
  {
    -- Move to left tab (tmux)
    key = 'LeftArrow',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="p", mods=""}),
      }),
  },
  {
    -- Move to right tab (wezterm)
    key = 'RightArrow',
    mods = 'CMD|OPT',
    action = wezterm.action.ActivateTabRelativeNoWrap(1)
  },
  {
    -- Move to right tab (tmux)
    key = 'RightArrow',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="n", mods=""}),
      }),
  },
  {
    -- Move tab to the left (wezterm)
    key = 'LeftArrow',
    mods = 'CMD|SHIFT',
    action = wezterm.action.MoveTabRelative(-1)
  },
  {
    -- Move tab to the right (wezterm)
    key = 'RightArrow',
    mods = 'CMD|SHIFT',
    action = wezterm.action.MoveTabRelative(1)
  },
  {
    key = '[',
    mods = 'CMD',
    action = wezterm.action.ActivateLastTab
  },
  {
    key = '.',
    mods = 'CMD',
    action = wezterm.action.ShowTabNavigator
  },
  {
    -- New tab (wezterm)
    key = 't',
    mods = 'CMD|OPT',
    action = wezterm.action{SpawnTab="CurrentPaneDomain"}
  },
  {
    -- New tab/window (tmux)
    key = 't',
    mods = 'CMD',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="c", mods=""}),
      }),
  },
  {
    -- Detatch tmux session
    key = 'd',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.Multiple({
        wezterm.action.SendKey({key="b", mods="CTRL"}),
        wezterm.action.SendKey({key="d", mods=""}),
      }),
  }
}

--config.color_scheme = 'dayfox'
--config.color_scheme = 'Atelier Estuary Light (base16)'
--config.color_scheme = 'Mariana'
config.color_scheme = 'Everforest Dark (Gogh)'

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.font_size = 16.0

config.colors = {
  background = "#414141",
  tab_bar = {
    background = "#3c3c3c",  -- Darker, warmer gray to sync better with the background
    active_tab = {
      bg_color = "#5a5a5a",  -- Slightly darker and warmer gray
      fg_color = "#e0e0e0",  -- Softer white to reduce contrast
      intensity = "Normal",
      underline = "None",
      italic = false,
      strikethrough = false,
    },
    inactive_tab = {
      bg_color = "#4a4a4a",  -- Darker and warmer gray for inactive tabs
      fg_color = "#a0a0a0",  -- Softer gray for text
      intensity = "Normal",
      underline = "None",
      italic = false,
      strikethrough = false,
    },
    inactive_tab_hover = {
      bg_color = "#5a5a5a",  -- Match active tab color to avoid stark contrast
      fg_color = "#e0e0e0",  -- Softer white to match active tab
      intensity = "Normal",
      underline = "None",
      italic = false,
      strikethrough = false,
    },
  },
}

config.enable_scroll_bar = true

-- Disable ligatures
config.harfbuzz_features = {"calt=0", "clig=0", "liga=0"}

--config.default_prog = { "/opt/homebrew/bin/tmux", "new-session" }

-- and finally, return the configuration to wezterm
return config

