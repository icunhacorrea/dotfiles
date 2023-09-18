
vim.cmd([[
    augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
]])

local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

packer.init({
    display = {
            open_fn = function()
                return require('packer.util').float({ border = 'single' })
            end
        }
    }
)

return require('packer').startup(function(use)

    use 'wbthomason/packer.nvim' -- Have packer manage itself

    use 'lukas-reineke/indent-blankline.nvim'

    use {
        'w0rp/ale',
        ft = {'sh', 'zsh', 'bash', 'c', 'cpp', 'cmake', 'html', 'markdown', 'racket', 'vim', 'tex', 'python'},
        cmd = 'ALEEnable',
        config = 'vim.cmd[[ALEEnable]]'
    }

    use {
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons', -- optional
        }
    }

    use 'hrsh7th/nvim-cmp' 
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-cmdline'
    use 'saadparwaiz1/cmp_luasnip'

    use 'L3MON4D3/LuaSnip'
    use 'rafamadriz/friendly-snippets'

    use {
        'windwp/nvim-autopairs',
        config = function() require("nvim-autopairs").setup {} end
    }
    
    use {
        'windwp/nvim-ts-autotag'
    }

    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true }
    }

    use 'nvim-tree/nvim-web-devicons'
    
    use {'romgrk/barbar.nvim', requires = 'nvim-web-devicons'}

    use {
        'nvim-treesitter/nvim-treesitter',
        run = function()
            local ts_update = require('nvim-treesitter.install').update({ with_sync = true }        )
            ts_update()
        end,
    }

    use { "folke/tokyonight.nvim", as = "tokyonight" }

    -- use { "rebelot/kanagawa.nvim", as = "kanagawa" }

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.1',
        requires = { {'nvim-lua/plenary.nvim'} }
    }

end)

