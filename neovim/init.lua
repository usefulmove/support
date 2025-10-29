-- ~/.config/nvim/init.lua

-- basic options (safe defaults)
vim.g.mapleader = " "                   -- space as leader
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.signcolumn = "yes"
vim.opt.cursorline = true
vim.opt.updatetime = 200
vim.opt.timeoutlen = 400
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = false
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.scrolloff = 6
vim.opt.clipboard = "unnamedplus"
vim.opt.laststatus = 3                  -- global statusline (no plugin needed)
vim.opt.undofile = true                 -- persistent undo
vim.opt.lazyredraw = true               -- optional: speed up renders on some terminals

-- bootstrap lazy.nvim (one-time)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim", lazypath
  })
end
vim.opt.rtp:prepend(lazypath)

-- plugins
require("lazy").setup({
  -- Kanagawa theme
  {
    "rebelot/kanagawa.nvim",
    name = "kanagawa",
    priority = 1000,
    opts = {
      compile = false,
      dimInactive = true,
      background = { dark = "dragon", light = "lotus" }, -- defaults
      overrides = function(colors)
        -- tweaks (optional)
        local theme = colors.theme
        return {
          LineNr = { fg = theme.ui.nontext },
          CursorLineNr = { fg = theme.ui.special, bold = true },
          Visual = { bg = theme.ui.bg_p1 },
        }
      end,
    },
    config = function(_, opts)
      require("kanagawa").setup(opts)
      -- default flavor:
      -- "kanagawa-wave", "kanagawa-dragon", or "kanagawa-lotus"
      vim.cmd.colorscheme("kanagawa-wave")
    end,
  },
}, {
  ui = { border = "rounded" },
})
