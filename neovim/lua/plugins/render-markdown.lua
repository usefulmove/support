return {
  'MeanderingProgrammer/render-markdown.nvim',
  dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-mini/mini.nvim' },
  opts = {
    -- Links: keep behavior but remove the hyperlink icon
    link = {
      hyperlink = '',  -- no icon before links
    },

    -- Checkboxes: keep your plain icons
    checkbox = {
      unchecked = { icon = ' ' },
      checked   = { icon = 'x' },
    },
  },
}
