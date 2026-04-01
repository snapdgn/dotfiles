return {
    -- Bufferline
    {
        'akinsho/bufferline.nvim',
        dependencies = 'nvim-tree/nvim-web-devicons'
    },

    -- Colorscheme
    {
        --'maxmx03/fluoromachine.nvim',
        --'folke/tokyonight.nvim',
        --'uloco/bluloco.nvim',
        --"romanaverin/charleston.nvim",
    },

    -- Lualine (THE NEW EXTENSION ADDED IN CONFIGURATION)
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        -- THIS IS THE CORRECT PLACE TO PUT LUALINE'S CONFIGURATION
        -- The `opts` table is passed directly to require('lualine').setup()
        opts = {
            options = {
                icons_enabled = true,
                theme = 'auto', -- 'auto' is usually good, or specify a theme like 'tokyonight'
                component_separators = { left = '', right = '' },
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
                lualine_a = { 'mode' },
                lualine_b = { 'branch', 'diff', 'diagnostics' },
                lualine_c = { 'filename' },
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
    },

    -- Which-key
    {
        'folke/which-key.nvim',
        lazy = true,
    },
}
