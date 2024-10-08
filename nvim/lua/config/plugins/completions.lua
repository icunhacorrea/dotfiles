local cmp = require'cmp'

require("luasnip.loaders.from_vscode").lazy_load()

local lspkind = require('lspkind')

cmp.setup({
    
    -- formatting = {
    --     format = lspkind.cmp_format({
    --         mode = "symbol_text",
    --         maxwidth = 50,
    --         ellipsis_char = "...",
    --         show_labelDetails = true
    --     }) 
    -- },

    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        end,
    },

    -- window = {
    --     completion = cmp.config.window.bordered(),
    --     documentation = cmp.config.window.bordered()
    -- },

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
