return {
  'MeanderingProgrammer/render-markdown.nvim',
  dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-mini/mini.nvim' },
  opts = {
    window = {
      conceal = {
        spell = true,
      },
    },
    headings = {
      level = { 1, 2, 3, 4, 5, 6 },
      border = false,
      icons = { '󰲡 ', '󰲣 ', '󰲥 ', '󰲧 ', '󰲩 ', '󰲫 ' }, -- keep icons, or set to {} to disable
    },
    code = {
      fence = true,
    },
    links = {
      inline = true,
    },
    block_quote = {
      enabled = true,
    },
    link = {
      hyperlink = '', -- remove hyperlink icon
    },
    checkbox = {
      unchecked = { icon = ' ' },
      checked = { icon = 'x' },
    },
  },
}
