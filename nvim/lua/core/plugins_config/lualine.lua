require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'zenburn',
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
