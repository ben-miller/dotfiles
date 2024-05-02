vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.wrap = false

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

--vim.opt.clipboard = "unnamedplus"

vim.opt.scrolloff = 999

vim.opt.virtualedit = "block"

vim.opt.inccommand = "split"

vim.opt.ignorecase = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 16

vim.api.nvim_set_hl(0, "Normal", { bg = "#f2e5bc" })

vim.cmd [[
  augroup ColorSchemeOverrides
    autocmd!
    autocmd ColorScheme * highlight Normal guibg=#f4f3ec
  augroup END
]]

-- Darken the background when not focused
if os.getenv("TMUX") then
    -- Use the Neovim API to create autocommands for focus events
    vim.api.nvim_create_autocmd("FocusGained", {
        pattern = "*",
        callback = function()
            -- Set lighter background when focused
            vim.api.nvim_set_hl(0, "Normal", { bg = "#f4f3ec" })
        end
    })

    vim.api.nvim_create_autocmd("FocusLost", {
        pattern = "*",
        callback = function()
            -- Set darker background when not focused
            vim.api.nvim_set_hl(0, "Normal", { bg = "#c5c5c1" })
        end
    })
end

-- Set 2-space indentation for HTML files
vim.api.nvim_create_autocmd("FileType", {
  pattern = "vue",
  callback = function()
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.expandtab = true
  end
})

