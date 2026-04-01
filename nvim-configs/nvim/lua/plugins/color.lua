-- Define a function to set the color scheme and common options.
-- This function can be called by 'cmd' in lazy.nvim or explicitly when you want to change schemes.
-- We no longer need separate functions for each color scheme if the logic is similar.
local function set_colorscheme(name, extra_highlights)
    vim.opt.background = "dark"
    vim.opt.termguicolors = true
    vim.cmd.colorscheme(name)

    -- Apply any common or specific extra highlights here
    if extra_highlights then
        for group, opts in pairs(extra_highlights) do
            vim.api.nvim_set_hl(0, group, opts)
        end
    end

    -- General common overrides to remove background from Normal and NormalFloat
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

return {
    -- Default/Primary Colorscheme
    -- This is the one that will load on startup.
    -- If you want a truly minimal startup, you can apply a very basic theme here
    -- or load 'onedark' only via a command/autocmd.
    {
        "navarasu/onedark.nvim",
        name = "onedark",
        priority = 1000, -- Give it high priority to ensure it loads before other things try to set colors
        config = function()
            -- Only the setup for onedark itself, if any, goes here.
            -- The actual setting of the colorscheme is moved to the utility function.
            -- If onedark has its own setup() function, call it here:
            -- require('onedark').setup { ... }
            -- Then set the colorscheme:
            set_colorscheme("onedark") -- Apply onedark as the default
        end,
    },

    --{
        --"morhetz/neovim",
        --name = "onedark",
        --priority = 1000, -- Give it high priority to ensure it loads before other things try to set colors
        --config = function()
            ---- Only the setup for onedark itself, if any, goes here.
            ---- The actual setting of the colorscheme is moved to the utility function.
            ---- If onedark has its own setup() function, call it here:
            ---- require('onedark').setup { ... }
            ---- Then set the colorscheme:
            --set_colorscheme("rose-pine") -- Apply onedark as the default
        --end,
    --},
    -- Lazy-load other color schemes. They will only be loaded when you explicitly
    -- run their respective `colorscheme` command.
    { "rose-pine/neovim", name = "rose-pine", lazy = true },
    { "morhetz/gruvbox", name = "gruvbox", lazy = true },
    { "folke/tokyonight.nvim", name = "tokyonight", lazy = true },
    { "w0ng/vim-hybrid", name = "hybrid", lazy = true },
    { "i3d/vim-jimbothemes", lazy = true }, -- This contains 'breakingbad'
    { "lurst/austere.vim", lazy = true },
    { "blazkowolf/gruber-darker.nvim", name = "gruber-darker", lazy = true },

    -- Colorizer is often needed early if you want real-time color highlighting.
    -- You might choose to keep it eager if it's essential for your immediate visual feedback.
    -- If you can delay it, use 'BufReadPre' or 'FileType'.
    -- For now, let's keep it eager as it's a visual utility.
    "chrisbra/Colorizer",
}
--function ColorMyAustere()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("austere")

    --vim.api.nvim_set_hl(0, "Constant", { fg = "#ce5252" })
    --vim.api.nvim_set_hl(0, "ColorColumn", { bg = "#252525" })
--end

--function ColorMyBreakingBad()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("breakingbad")

    --vim.api.nvim_set_hl(0, "ColorColumn", { bg = "#404040" })
    --vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    --vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--end

--function ColorMyGruvbox()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("gruvbox")

    --vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    --vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--end

--function ColorMyRosePine()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("rose-pine")

    --vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    --vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--end

--function ColorMyGruber()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("gruber-darker")

    --vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    --vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--end

--function ColorMyOneDark()
    --vim.opt.background = "dark"
    --vim.opt.termguicolors = true
    --vim.cmd.colorscheme("onedark")

    --vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    --vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--end

--return {
    --{ "rose-pine/neovim",      name = "rose-pine" },
    ----{ "morhetz/gruvbox",       name = "gruvbox", config = ColorMyGruvbox },
    --{ "folke/tokyonight.nvim", name = "tokyonight" },
    --{ "w0ng/vim-hybrid",       name = "hybrid" },
    --{ "navarasu/onedark.nvim",      name = "onedark", config = ColorMyOneDark},
    --"i3d/vim-jimbothemes",
    --"chrisbra/Colorizer",
    --"lurst/austere.vim",
    ----{ "blazkowolf/gruber-darker.nvim", name = "gruber-darker", config = ColorMyGruber },
--}
