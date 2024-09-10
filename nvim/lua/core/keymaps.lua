local keymap = vim.keymap
local opts = { noremap = true, silent = true }

keymap.set('n', '<leader>l', ':Lazy<Return>', opts)
keymap.set('n', '<leader>m', ':Mason<Return>', optq)

keymap.set('n', '<leader>ss', ':split<Return>', opts)
keymap.set('n', '<leader>sv', ':vsplit<Return>', opts)
keymap.set('n', '<leader>sw', '<C-w>w', opts)

keymap.set('n', '<leader>p', '<Cmd>BufferPrevious<CR>', opts)
keymap.set('n', '<leader>n', '<Cmd>BufferNext<CR>', opts)
keymap.set('n', '<leader>q', '<Cmd>BufferClose<CR>', opts)
