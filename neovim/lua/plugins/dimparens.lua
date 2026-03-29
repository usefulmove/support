return {
  {
    "dimparens.nvim",
    -- absolute path to local development directory
    dir = "/home/dedmonds/repos/dimparens.nvim",
    event = "VeryLazy",
    opts = {
      languages = { "lua", "python", "scheme", "cpp", "rust", "odin" },
    },
    config = function(_, opts)
      require("dimparens").setup(opts)
    end,
  },
}
