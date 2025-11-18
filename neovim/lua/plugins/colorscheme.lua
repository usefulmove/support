return {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    opts = {},
    config= function()
        vim.cmd.colorscheme "catppuccin"
    end,
}

--[[
return {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
    config= function()
        vim.cmd.colorscheme "kanagawa-wave"
    end,
}
]]
