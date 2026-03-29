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
            ensure_installed = { "lua_ls", "rust_analyzer", "pyright", "clangd" }
        })

        -- lua
        vim.lsp.enable("lua_ls")

        -- rust
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
        vim.lsp.enable("rust_analyzer")

        -- python
        vim.lsp.enable("pyright")

        -- c/c++
        vim.lsp.enable("clangd")

        -- odin
        vim.lsp.enable("ols")
    end
}
