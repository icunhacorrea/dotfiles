require("lazy").setup({

    -- Dependencies
    { "nvim-tree/nvim-web-devicons" },
    { "nvim-lua/plenary.nvim" },
    { "MunifTanjim/nui.nvim" },
    
    -- Plugins
    { "phha/zenburn.nvim" },
    { "nvim-tree/nvim-tree.lua" },
    { "nvim-lualine/lualine.nvim" },
    { "lewis6991/gitsigns.nvim" },
    { "windwp/nvim-ts-autotag" },
    { "windwp/nvim-autopairs" },
    { "romgrk/barbar.nvim" },
    { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },
    { "nvim-telescope/telescope.nvim", tag = "0.1.6" },
    { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
    { "VonHeikemen/fine-cmdline.nvim" },

    -- Completion
    { "hrsh7th/nvim-cmp" },
    { "onsails/lspkind.nvim" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "L3MON4D3/LuaSnip" },
    { "saadparwaiz1/cmp_luasnip" },
    { "rafamadriz/friendly-snippets" },
    { "williamboman/mason.nvim" },
    { "williamboman/mason-lspconfig.nvim" },
    { "neovim/nvim-lspconfig" }

})
