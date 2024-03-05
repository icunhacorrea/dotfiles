--[[

=====================================================================
====================  Iago Correa nvim Dotfile  ====================
=====================================================================

--]]

require("plugins")

require("ibl").setup {
    scope = { enabled = false },
}

-- Find python3
local python_path = vim.fn.exepath("python3")
if python_path == nil or python_path == "" then
  python_path = vim.g.homebrew_install_dir .. "/bin/python3"
  if utils.file_or_dir_exists(python_path) then
    vim.g.python3_host_prog = python_path
  end
else
  vim.g.python3_host_prog = python_path
end

require("mason").setup()
require("mason-lspconfig").setup()

local servers = {
    pyright = { }
}

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    }
  end,
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
        { name = 'nvim_lsp' },
        { name = 'luasnip' } -- For luasnip users.
    }, {
        { name = 'buffer' },
    })
})

-- LSP config 

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
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
    options = { theme = 'horizon' }
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

-- Lua
require("rose-pine").setup({
    variant = "auto", -- auto, main, moon, or dawn
    dark_variant = "moon", -- main, moon, or dawn
    dim_inactive_windows = false,
    extend_background_behind_borders = true,

    enable = {
        terminal = true,
        legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
        migrations = true, -- Handle deprecated options automatically
    },

    styles = {
        bold = true,
        italic = true,
        transparency = false,
    },

    groups = {
        border = "muted",
        link = "iris",
        panel = "surface",

        error = "love",
        hint = "iris",
        info = "foam",
        note = "pine",
        todo = "rose",
        warn = "gold",

        git_add = "foam",
        git_change = "rose",
        git_delete = "love",
        git_dirty = "rose",
        git_ignore = "muted",
        git_merge = "iris",
        git_rename = "pine",
        git_stage = "iris",
        git_text = "rose",
        git_untracked = "subtle",

        h1 = "iris",
        h2 = "foam",
        h3 = "rose",
        h4 = "gold",
        h5 = "pine",
        h6 = "foam",
    },

    highlight_groups = {
        -- Comment = { fg = "foam" },
        -- VertSplit = { fg = "muted", bg = "muted" },
    },

    before_highlight = function(group, highlight, palette)
        -- Disable all undercurls
        -- if highlight.undercurl then
        --     highlight.undercurl = false
        -- end
        --
        -- Change palette colour
        -- if highlight.fg == palette.pine then
        --     highlight.fg = palette.foam
        -- end
    end,
})

-- Set colorscheme after options
vim.cmd('colorscheme rose-pine')

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
vim.o.autoindent = true
vim.o.smarttab= true

vim.o.nu = true

vim.o.hls = true
vim.o.scs = true
vim.o.ic = true
vim.o.is = true

vim.o.swapfile = false
vim.o.backup = false

vim.o.splitright = true
vim.o.splitbelow = true

vim.cmd "set listchars=eol:!,tab:>=,trail:."
vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd "set termguicolors"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1


-- keymaps

local builtin = require('telescope.builtin')

local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
end

-- LSP Mappings
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

vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
