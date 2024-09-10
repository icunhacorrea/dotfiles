require'nvim-treesitter.configs'.setup {
    ensure_installed = { "lua", "vim", "bash", "html", "python" },

    sync_install = false,

    ignore_install = { "javascript" },

    highlight = {
        enable = true,

        adicional_vim_regex_highlighting = false
    },

    indent = {
        enable = true
    }
}
