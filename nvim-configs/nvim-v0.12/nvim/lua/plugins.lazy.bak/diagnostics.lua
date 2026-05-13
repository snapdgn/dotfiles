return {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    config = function()
        require('tiny-inline-diagnostic').setup({
            signs = {
                left = "",
                right = "",
                diag = "●",
                arrow = "    ",
                up_arrow = "    ",
                vertical = " │",
                vertical_end = " └"
            },
            options = {
                show_source = false,
                throttle = 20,
                multiple_diag_under_cursor = false,
            }
        })
    end
}
