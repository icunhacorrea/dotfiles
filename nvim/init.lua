--[[

=====================================================================
====================  Iago Correa nvim Dotfile  ====================
=====================================================================

--]]

require("plugins")

require("ibl").setup {
    scope = { enabled = false }
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

require('rose-pine').setup({
	--- @usage 'auto'|'main'|'moon'|'dawn'
	variant = 'moon',
	--- @usage 'main'|'moon'|'dawn'
	dark_variant = 'moon',
	bold_vert_split = false,
	dim_nc_background = false,
	disable_background = false,
	disable_float_background = false,
	disable_italics = true,

	--- @usage string hex value or named color from rosepinetheme.com/palette
	groups = {
		background = 'base',
		background_nc = '_experimental_nc',
		panel = 'surface',
		panel_nc = 'base',
		border = 'highlight_med',
		comment = 'muted',
		link = 'iris',
		punctuation = 'subtle',

		error = 'love',
		hint = 'iris',
		info = 'foam',
		warn = 'gold',

		headings = {
			h1 = 'iris',
			h2 = 'foam',
			h3 = 'rose',
			h4 = 'gold',
			h5 = 'pine',
			h6 = 'foam',
		}
		-- or set all headings at once
		-- headings = 'subtle'
	},

	-- Change specific vim highlight groups
	-- https://github.com/rose-pine/neovim/wiki/Recipes
	highlight_groups = {
		ColorColumn = { bg = 'rose' },

		-- Blend colours against the "base" background
		CursorLine = { bg = 'foam', blend = 10 },
		StatusLine = { fg = 'love', bg = 'love', blend = 10 },

		-- By default each group adds to the existing config.
		-- If you only want to set what is written in this config exactly,
		-- you can set the inherit option:
		Search = { bg = 'gold', inherit = false },
	}
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
