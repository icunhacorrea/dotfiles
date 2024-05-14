local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
end

vim.api.nvim_set_keymap('n', ':', '<cmd>FineCmdline<CR>', {noremap = true})

vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

vim.keymap.set('n', '<C-t>', ':NvimTreeToggle<CR>', opts('Toggle'))
vim.keymap.set('n', '<leader>n',':NvimTreeFocus<CR>', opts('Focus'))
vim.keymap.set('n', '<C-n>',':NvimTreeOpen<CR>', opts('Open'))
vim.keymap.set('n', '<C-f>',':NvimTreeFindFile<CR>', opts('Find'))

vim.keymap.set('n', '<F1>', '<Cmd>BufferPrevious<CR>', opts('BufferPrevious'))
vim.keymap.set('n', '<F2>', '<Cmd>BufferNext<CR>', opts('BufferNext'))
vim.keymap.set('n', '<F3>', '<Cmd>BufferClose<CR>', opts('BufferClose'))
