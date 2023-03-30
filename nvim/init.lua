--[[

=====================================================================
====================  Iago Correa nvim Dotfile  ====================
=====================================================================

--]]

require("plugins")

require("indent_blankline").setup {
    char = '┊',
    show_trailing_blankline_indent = false,
}

local cmp = require'cmp'
cmp.setup({
    snippet = {
    expand = function(args)
        require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
        { name = 'luasnip' }, -- For luasnip users.
    }, {
        { name = 'buffer' },
    })
})


-- OR setup with some options
require("nvim-tree").setup({
    sort_by = "case_sensitive",
    renderer = {
        group_empty = true,
    },
    filters = {
        dotfiles = true,
    },
})

require('lualine').setup({
    options = { theme = 'dracula' }
})

-- Treesitter work around

vim.api.nvim_create_autocmd({'BufEnter','BufAdd','BufNew','BufNewFile','BufWinEnter'}, {
    group = vim.api.nvim_create_augroup('TS_FOLD_WORKAROUND', {}),
    callback = function()
        vim.opt.foldmethod     = 'expr'
        vim.opt.foldexpr       = 'nvim_treesitter#foldexpr()'
    end
})

-- Treesitter autotag

require'nvim-treesitter.configs'.setup {
    autotag = {
        enable = true,
    }
}

-- Vim configurations:

vim.g.mapleader = '\\'
vim.g.maplocalleader = '\\'

vim.o.mouse = 'a'

vim.o.ai = true

vim.o.sm = true

vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.smartindent = true

vim.o.nu = true

vim.o.hls = true
vim.o.scs = true
vim.o.ic = true
vim.o.is = true

vim.o.swapfile = false
vim.o.backup = false

vim.o.splitright = true
vim.o.splitbelow = true

vim.cmd "set whichwrap+=<,>,[,],h,l"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Nvim Tree keymaps

vim.keymap.set('n', '<C-t>', ':lua require("nvim-tree.api").tree.toggle(false, true)<CR>') 
vim.keymap.set('n', '<leader>n',':NvimTreeFocus<CR>')
vim.keymap.set('n', '<C-n>',':NvimTreeOpen<CR>')
vim.keymap.set('n', '<C-f>',':NvimTreeFindFile<CR>')

