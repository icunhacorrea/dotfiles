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
    options = { theme = 'wombat' }
})


-- Treesitter autotag

require'nvim-treesitter.configs'.setup {
    ensure_installed = { "c", "lua", "cpp", "java", "bash", "html" },

    sync_install = false,

    ignore_install = { "javascript" },

    highlight = {
        enable = true,

        disable = { "c", "bash" },

        adicional_vim_regex_highlighting = false
    },

    autotag = {
        enable = true,
        filetypes = { "html" , "xml" },
    }
}

require('nvim-ts-autotag').setup()

-- Default options:
require('kanagawa').setup({
    compile = false,             -- enable compiling the colorscheme
    undercurl = true,            -- enable undercurls
    commentStyle = { italic = true },
    functionStyle = {},
    keywordStyle = { italic = true},
    statementStyle = { bold = true },
    typeStyle = {},
    transparent = true,         -- do not set background color
    dimInactive = false,         -- dim inactive window `:h hl-NormalNC`
    terminalColors = true,       -- define vim.g.terminal_color_{0,17}
    colors = {                   -- add/modify theme and palette colors
        palette = {},
        theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
    },
    overrides = function(colors) -- add/modify highlights
        return {}
    end,
    theme = "wave",              -- Load "wave" theme when 'background' option is not set
    background = {               -- map the value of 'background' option to a theme
        dark = "wave",           -- try "dragon" !
        light = "lotus"
    },
})

vim.cmd("colorscheme kanagawa")

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

-- keymaps

local builtin = require('telescope.builtin')

local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
end

vim.keymap.set('n', '<C-t>', ':lua require("nvim-tree.api").tree.toggle(false, true)<CR>', opts('Toggle')) 
vim.keymap.set('n', '<leader>n',':NvimTreeFocus<CR>', opts('Focus'))
vim.keymap.set('n', '<C-n>',':NvimTreeOpen<CR>', opts('Open'))
vim.keymap.set('n', '<C-f>',':NvimTreeFindFile<CR>', opts('Find'))

vim.keymap.set('n', '<A-,>', '<Cmd>BufferPrevious<CR>', opts('BufferPrevious'))
vim.keymap.set('n', '<A-.>', '<Cmd>BufferNext<CR>', opts('BufferNext'))
vim.keymap.set('n', '<A-c>', '<Cmd>BufferClose<CR>', opts('BufferClose'))

vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

