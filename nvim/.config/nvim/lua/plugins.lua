local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    {
        "https://github.com/rebelot/kanagawa.nvim",
        config = function()
            vim.cmd.colorscheme("kanagawa-dragon")
        end
    },
    {
        "https://github.com/nvim-treesitter/nvim-treesitter",
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = { "c", "lua", "vim", "vimdoc", "query" },

                auto_install = true,

                highlight = {
                    enable = true
                },
            })
        end
    },
    --{
    --    "https://github.com/github/copilot.vim.git",
    --    config = function()
    --    end
    --},
    {
        "toppair/peek.nvim",
        event = { "VeryLazy" },
        build = "deno task --quiet build:fast",
        config = function()
            require("peek").setup()
            -- refer to `configuration to change defaults`
            vim.api.nvim_create_user_command("PeekOpen", require("peek").open, {})
            vim.api.nvim_create_user_command("PeekClose", require("peek").close, {})
        end,
    },
    {
        "https://github.com/tpope/vim-commentary",
        config = function()
            vim.api.nvim_set_keymap("n", "<leader>/", "<Plug>CommentaryLine", {})
            vim.api.nvim_set_keymap("v", "<leader>/", "<Plug>Commentary", {})
        end
    },
    {
        "https://github.com/tidalcycles/vim-tidal",
        config = function()
        end
    },
    {
        "https://github.com/supercollider/scvim",
        config = function()
        end
    },{
        "https://github.com/atelierbram/vim-colors_atelier-schemes",
        config = function()
        end
    }
})

