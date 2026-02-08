-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

vim.opt.number = false
vim.opt.relativenumber = false

vim.opt.cursorline = false

vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#1b2b34", fg = "#d8dee9" })
vim.api.nvim_set_hl(0, "FloatBorder", { bg = "#1b2b34", fg = "#a7adba" })
vim.api.nvim_set_hl(0, "DiagnosticError", { fg = "#ec5f67" })

require("nvim-treesitter").setup({
  -- A list of parser names, or "all" (the five listed parsers should always be installed)
  ensure_installed = { "c", "lua", "vim", "vimdoc", "python", "javascript", "ruby", "rust", "zig" }, -- Add/remove languages here
  -- Install parsers synchronously (only used for first install, defaults to false)
  sync_install = false,
  -- Automatically install parsers when opening a file of an unknown language
  auto_install = true,

  highlight = {
    enable = true, -- enables treesitter highlight
    -- disable = { "c", "lua" }, -- list of language that will be disabled
  },
})

vim.o.guifont = "Source Code Pro:h8.5" -- text below applies for VimScript

vim.lsp.enable({ "ruby_lsp", "rust-analyzer", "zls" })

require("colorizer").setup()
