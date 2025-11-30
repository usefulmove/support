return {
    "mason-org/mason-lspconfig.nvim",
    opts = {},
    dependencies = {
        {
            "mason-org/mason.nvim",
            opts = {}
        },
        "neovim/nvim-lspconfig",
    },
    config = function()
        require("mason-lspconfig").setup({
            ensure_installed = { "lua_ls", "rust_analyzer", "pyright" }
        })

        vim.lsp.enable("lua_ls")

        vim.lsp.config('rust_analyzer', {
          settings = {
            ['rust-analyzer'] = {
              cargo = {
                allFeatures = true,
              },
              procMacro = {
                enable = true,
              },
            },
          },
        })

        vim.lsp.enable("pyright")
    end
}
