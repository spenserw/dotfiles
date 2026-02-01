-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

vim.opt.number = false
vim.opt.relativenumber = false

vim.opt.cursorline = false

require("nvim-treesitter").setup({
  -- A list of parser names, or "all" (the five listed parsers should always be installed)
  ensure_installed = { "c", "lua", "vim", "vimdoc", "python", "javascript", "ruby" }, -- Add/remove languages here
  -- Install parsers synchronously (only used for first install, defaults to false)
  sync_install = false,
  -- Automatically install parsers when opening a file of an unknown language
  auto_install = true,

  highlight = {
    enable = true, -- enables treesitter highlight
    -- disable = { "c", "lua" }, -- list of language that will be disabled
  },
})

vim.lsp.enable("ruby_lsp")

require("colorizer").setup()
