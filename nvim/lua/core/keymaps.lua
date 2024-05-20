local keymap = vim.keymap
local opts = { noremap = true, silent = true }

local function opts_nvimtree(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
end

vim.api.nvim_set_keymap('n', ':', '<cmd>FineCmdline<CR>', {noremap = true})

keymap.set('n', '<space>e', vim.diagnostic.open_float)
keymap.set('n', '[d', vim.diagnostic.goto_prev)
keymap.set('n', ']d', vim.diagnostic.goto_next)
keymap.set('n', '<space>q', vim.diagnostic.setloclist)

keymap.set('n', '<C-t>', ':NvimTreeToggle<CR>', opts_nvimtree('Toggle'))
keymap.set('n', '<leader>n',':NvimTreeFocus<CR>', opts_nvimtree('Focus'))
keymap.set('n', '<C-n>',':NvimTreeOpen<CR>', opts_nvimtree('Open'))
keymap.set('n', '<C-f>',':NvimTreeFindFile<CR>', opts_nvimtree('Find'))

keymap.set('n', '<s-tab>', '<Cmd>BufferPrevious<CR>', opts)
keymap.set('n', '<tab>', '<Cmd>BufferNext<CR>', opts)
keymap.set('n', 'q<tab>', '<Cmd>BufferClose<CR>', opts)

keymap.set('n', 'ss', ':split<Return>', opts)
keymap.set('n', 'sv', ':vsplit<Return>', opts)
keymap.set('n', 'sw', '<C-w>w', opts)

keymap.set('n', '<leader>l', ':Lazy<Return>', opts)
keymap.set('n', '<leader>m', ':Mason<Return>', optq)
