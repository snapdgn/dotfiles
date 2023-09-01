active = false
function nmap(keys, command)
  vim.api.nvim_set_keymap("n", keys, command .. " <CR>", { noremap = true, silent = true })
end

function vmap(keys, command)
  vim.api.nvim_set_keymap("v", keys, command .. " <CR>", { noremap = true, silent = true })
end

function minimal()
  if active then
    vim.cmd [[
      set number relativenumber backspace=0 noshowmode showtabline=1 laststatus=2 signcolumn=yes foldcolumn=0 
      au WinEnter,BufEnter, * set number relativenumber 
    ]]
    active = false
  else 
    vim.cmd [[
      set number norelativenumber showmode showtabline=0 laststatus=0 signcolumn=no foldcolumn=1
      au WinEnter,BufEnter, * set nonumber norelativenumber 
    ]]
    active = true
  end
end



-- Formatter
vim.api.nvim_set_keymap("n", "<leader>f", ":Format<CR>", { noremap = true, silent = true, })
vim.api.nvim_set_keymap("n", "<leader>F", ":FormatWrite<CR>", { noremap = true, silent = true, })
-- Format after save
-- vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
--nmap("<C-j>", ":<ESC>")

vim.api.nvim_set_keymap('i', '<C-j>', '<Esc>', { noremap = true, })
vim.api.nvim_set_keymap("", "<leader>cc", ":NERDCommenterComment", { })
vim.api.nvim_set_keymap("", "<leader>cu", ":NERDCommenterUncomment", { })
vim.api.nvim_set_keymap("", "<leader>m", ":MarkdownPreview<CR>", { })

-- Disable arrow keys
vim.api.nvim_set_keymap("n", "<up>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("n", "<down>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<up>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<down>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<left>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<right>", "<nop>", { noremap = true, })

vim.api.nvim_set_keymap("v", "<up>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("v", "<down>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("v", "<left>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("v", "<right>", "<nop>", { noremap = true, })

-- No Backspace/Delete/PageUp/PageDown keys
vim.api.nvim_set_keymap("n", "<delete>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<delete>", "<nop>", { noremap = true, })

vim.api.nvim_set_keymap("n", "<PageUp>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<PageUp>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("v", "<PageUp>", "<nop>", { noremap = true, })


vim.api.nvim_set_keymap("n", "<PageDown>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("i", "<PageDown>", "<nop>", { noremap = true, })
vim.api.nvim_set_keymap("v", "<PageDown>", "<nop>", { noremap = true, })

-- Make double-<ESC> clear search highlights
vim.api.nvim_set_keymap("n", "<Esc><Esc>", "<Esc>:nohlsearch<CR><Esc>", { silent = true, noremap = true, })

--# copilot

-- Note: <TAB><TAB> keybindings, slows down the dropdown suggestion selection
--vim.api.nvim_set_keymap("i", "<TAB><TAB>", "<Plug>(copilot-next)", { silent = true, })
--vim.api.nvim_set_keymap("i", "<C-k>", "<Plug>(copilot-previous)", { silent = true, })
--vim.api.nvim_set_keymap("i", "<C-\\>", "<Plug>(copilot-dismiss)", { silent = true, })


-- Normal Map
nmap("<TAB>", ":tabnext")
nmap("<S-TAB>", ":tabprev")
--nmap("<C-H>", ":split")
--nmap("<C-S-v>", ":vs")
--nmap("<C-v>", ":vs +terminal | startinsert")
--nmap("<C-h>", ":split +terminal | startinsert")
nmap("<C-t>", ":tabnew")

vim.cmd("set guitablabel=%t\\ %m")

--nmap("<C-q>", ":q")
--nmap("<C-s>", ":w")

--nmap("<C-z>", ":u")
--nmap("<C-r>", ":redo")
nmap("<Leader>,", ":w<CR>")
nmap("<Leader>m", ":MarkdownPreview<CR>")

-- Minimal toggle
nmap("<C-m>", ":lua minimal()")

-- Telescope
nmap("<C-space>", ":Telescope")
nmap("<C-f>", ":Telescope find_files")
nmap("<C-b>", ":Telescope buffers")

--Quickly Switch buffers
vim.api.nvim_set_keymap("", "gn", ":bn<cr>", { })
vim.api.nvim_set_keymap("", "gp", ":bp<cr>", { })

-- NvimTree
nmap("<Leader>k", ":NvimTreeToggle")
--nmap("<C-n>", ":NvimTreeFocus")

-- Visual Map
vmap("<C-/>", ":lua require('Comment.api').toggle_linewise_op(vim.fn.visualmode())")
vmap("<C-d>", ":d")
vmap("<C-y>", ":y")

-- Code navigation shortcuts
vim.api.nvim_set_keymap("n", "<c-]>", "<cmd>lua vim.lsp.buf.definition()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.implementation()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "<c-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "1gD", "<cmd>lua vim.lsp.buf.type_definition()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "g0", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "gW", "<cmd>lua vim.lsp.buf.workspace_symbol()<CR>", { silent = true, noremap = true, })
vim.api.nvim_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { silent = true, noremap = true, })

-- toggle terminal
--
nmap("<Leader>t", ":ToggleTerm size=50")



--code_action
vim.api.nvim_set_keymap("n", "ga", "<cmd>lua vim.lsp.buf.code_action()<CR>", { silent = true, noremap = true, })

-- Rust run
nmap("<Leader>r", ":RustRun")
