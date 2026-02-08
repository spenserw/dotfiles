-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<C-q>", "<cmd>bd<cr>", { desc = "Delete buffer" })
vim.keymap.set("n", "<C-x>b", "<cmd>Telescope buffers<cr>", { desc = "Switch buffer" })
vim.keymap.set("n", "<leader>sc", function()
  Snacks.scratch()
end, { desc = "Scratch buffer" })

vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Navigate to pane below" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Navigate to pane above" })

vim.keymap.set("n", "<C-x>f", function()
  local dir = vim.fn.simplify(vim.fn.fnamemodify(vim.fn.expand("%:h"), ":."))
  require("telescope").extensions.file_browser.file_browser({
    path = dir ~= "." and dir or vim.loop.cwd(),
    path_display = { "smart" },
  })
end, { desc = "Browse files in current directory" })

vim.keymap.set("n", "<Tab>", "==", { desc = "Auto-indent current line" })
vim.keymap.set("i", "<S-Tab>", "<C-d>", { desc = "Unindent" })

vim.keymap.set("i", "<C-f>", "<Esc>", { desc = "Escape" })

vim.keymap.set("n", "<C-S-h>", "<cmd>BufferLineMovePrev<cr>", { desc = "Move tab left" })
vim.keymap.set("n", "<C-S-l>", "<cmd>BufferLineMoveNext<cr>", { desc = "Move tab right" })
