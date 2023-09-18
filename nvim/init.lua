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
    ensure_installed = { "c", "lua", "cpp", "java", "bash", "html", "python" },

    sync_install = false,

    ignore_install = { "javascript" },

    highlight = {
        enable = true,

        adicional_vim_regex_highlighting = false
    },

    autotag = {
        enable = true,
        filetypes = { "html" , "xml" },
    }
}

require('nvim-ts-autotag').setup()

require("tokyonight").setup({
    -- your configuration comes here
    -- or leave it empty to use the default settings
    style = "storm", -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
    light_style = "day", -- The theme is used when the background is set to light
    transparent = false, -- Enable this to disable setting the background color
    terminal_colors = true, -- Configure the colors used when opening a `:terminal` in [Neovim](https://github.com/neovim/neovim)
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
        -- Background styles. Can be "dark", "transparent" or "normal"
        sidebars = "dark", -- style for sidebars, see below
        floats = "dark", -- style for floating windows
    },
    sidebars = { "qf", "help" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
    day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
    hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
    dim_inactive = false, -- dims inactive windows
    lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold

    --- You can override specific color groups to use other groups or a hex color
    --- function will be called with a ColorScheme table
    ---@param colors ColorScheme
    on_colors = function(colors) end,

    --- You can override specific highlights to use other groups or a hex color
    --- function will be called with a Highlights and ColorScheme table
    ---@param highlights Highlights
    ---@param colors ColorScheme
    on_highlights = function(highlights, colors) end,
})

-- setup must be called before loading
vim.cmd.colorscheme "tokyonight"

-- Vim configurations:

vim.g.mapleader = '\\'
vim.g.maplocalleader = '\\'

vim.o.mouse = 'a'

vim.o.ai = true

vim.o.sm = true

vim.o.noexpandtab = true
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

-- vim.keymap.set('n', '<C-t>', ':lua require("nvim-tree.api").tree.toggle(false, true)<CR>', opts('Toggle')) 
vim.keymap.set('n', '<C-t>', ':NvimTreeToggle<CR>', opts('Toggle'))
vim.keymap.set('n', '<leader>n',':NvimTreeFocus<CR>', opts('Focus'))
vim.keymap.set('n', '<C-n>',':NvimTreeOpen<CR>', opts('Open'))
vim.keymap.set('n', '<C-f>',':NvimTreeFindFile<CR>', opts('Find'))

vim.keymap.set('n', '<F1>', '<Cmd>BufferPrevious<CR>', opts('BufferPrevious'))
vim.keymap.set('n', '<F2>', '<Cmd>BufferNext<CR>', opts('BufferNext'))
vim.keymap.set('n', '<F3>', '<Cmd>BufferClose<CR>', opts('BufferClose'))

vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

