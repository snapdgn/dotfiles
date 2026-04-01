-- Helper function to apply colorscheme with common settings
local function apply_colorscheme(name, opts)
    opts = opts or {}
    local background = opts.background or "dark"
    local transparent = opts.transparent == nil and true or opts.transparent
    local custom_highlights = opts.highlights or {}

    return function()
        vim.opt.background = background
        vim.opt.termguicolors = true
        vim.cmd.colorscheme(name)

        -- Apply transparent background
        if transparent then
            vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
            vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        end

        -- Apply custom highlights
        for group, hl in pairs(custom_highlights) do
            vim.api.nvim_set_hl(0, group, hl)
        end
    end
end

return {
    -- Default colorscheme (loads on startup)
    {
        "folke/tokyonight.nvim",
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                style = "moon",
                transparent = true,
            })
            vim.cmd.colorscheme("tokyonight-moon")
        end,
    },

    -- Alternative colorschemes (lazy-loaded, use :colorscheme command to switch)
    {
        "EdenEast/nightfox.nvim",
        lazy = true,
        config = apply_colorscheme("nordfox"),
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        lazy = true,
        opts = {
            flavour = "frappe",
            transparent_background = true,
        },
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        lazy = true,
        opts = {
            variant = "main",
            disable_background = true,
        },
    },
    {
        "rebelot/kanagawa.nvim",
        lazy = true,
        opts = {
            transparent = true,
            theme = "dragon",
        },
    },
    {
        "sainnhe/everforest",
        lazy = true,
        config = apply_colorscheme("everforest"),
    },
    {
        "sonph/onehalf",
        lazy = true,
        config = function(plugin)
            vim.opt.runtimepath:append(plugin.dir .. "/vim")
            vim.opt.background = "dark"
            vim.opt.termguicolors = true
            vim.cmd.colorscheme("onehalfdark")
            
            vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
            vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        end,
    },
}
