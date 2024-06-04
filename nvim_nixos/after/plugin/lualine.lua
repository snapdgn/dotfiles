require('lualine').setup {
    options = {
        icons_enabled = true,
        theme = 'gruvbox',
        component_separators = { left = '', right = '' },
        --component_separators = { left = '|', right = '|' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {
            statusline = {},
            winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        }
    },
    sections = {
        --lualine_a = { 'mode' },
        --sections = {
        lualine_a = {
            {
                'mode',
                icons_enabled = true, -- Enables the display of icons alongside the component.
                -- Defines the icon to be displayed in front of the component.
                -- Can be string|table
                -- As table it must contain the icon as first entry and can use
                -- color option to custom color the icon. Example:
                --{ 'branch', icon = '' } /
                { 'branch', icon = { '', color = { fg = 'green' } } },
                padding = 1, -- Adds padding to the left and right of components.
            }
        },
        lualine_b = { 'branch', { 'diff', colored = true }, 'diagnostics' },
        lualine_c = { 'filename', 'location' },
        lualine_x = { 'encoding', 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { 'filename' },
        lualine_x = { 'location' },
        lualine_y = {},
        lualine_z = {}
    },
    tabline = {},
    winbar = {},
    inactive_winbar = {},
    extensions = {}
}
