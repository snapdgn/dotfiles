-- ==========================================================================
-- 1. Modern Native LSP Configuration (Neovim 0.11 - 0.13+)
-- ==========================================================================

-- Use vim.lsp.config to modify the rust_analyzer definition
vim.lsp.config('rust_analyzer', {
  settings = {
    ["rust-analyzer"] = {
      inlayHints = {
        bindingModeHints = { enable = true },
        chainingHints    = { enable = true },
        closingBraceHints = { enable = true },
        parameterHints   = { enable = true },
        typeHints        = { enable = true },
      },
    },
  },
})

-- Start and auto-activate rust_analyzer for matching buffers
vim.lsp.enable('rust_analyzer')

-- ==========================================================================
-- 2. Buffer Attach Lifecycle & Global Settings
-- ==========================================================================

-- The modern replacement for 'on_attach' is an LspAttach autocommand
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    
    -- Dynamically enable inlay hints when the buffer attaches
    if client and client.supports_method("textDocument/inlayHint") then
      vim.lsp.inlay_hint.enable(true, { bufnr = ev.buf })
    end
  end,
})

-- Style the hints so they match your comments seamlessly
vim.api.nvim_set_hl(0, "LspInlayHint", { link = "Comment" })

-- Dynamic hotkey to toggle type hints on and off with <leader>th
vim.keymap.set("n", "<leader>th", function()
  local is_enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = 0 })
  vim.lsp.inlay_hint.enable(not is_enabled, { bufnr = 0 })
  print("Inlay Hints: " .. (not is_enabled and "ON" or "OFF"))
end, { desc = "Toggle LSP Inlay Hints" })

