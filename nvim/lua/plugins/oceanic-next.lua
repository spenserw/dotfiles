return {
  {
    "spenserw/oceanic-next",
    lazy = false, -- load immediately for colorschemes
    priority = 1000, -- ensure it loads before other plugins
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "OceanicNext",
    },
  },
}
