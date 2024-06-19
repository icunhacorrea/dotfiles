require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'modus-vivendi',
  },
  sections = {
    lualine_a = {
      {
        'filename',
        path = 1,
      }
    }
  }
}
