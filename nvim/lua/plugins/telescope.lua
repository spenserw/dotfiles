return {
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-telescope/telescope-file-browser.nvim" },
    config = function(_, opts)
      local telescope = require("telescope")

      opts.extensions = {
        file_browser = {
          display_stat = false,
          grouped = true,
        },
      }
      telescope.setup(opts)
      telescope.load_extension("file_browser")
    end,
    opts = {
      defaults = {
        layout_config = { prompt_position = "top" },
        sorting_strategy = "ascending",
      },
      pickers = {
        buffers = {
          sort_mru = true,
          entry_maker = function(entry)
            local Path = require("plenary.path")
            local utils = require("telescope.utils")

            local bufname = entry.info.name ~= "" and entry.info.name or "[No Name]"
            local path = Path:new(bufname):make_relative(vim.loop.cwd())

            return {
              value = entry,
              ordinal = path,
              display = function(e)
                return utils.transform_path({ path_display = { "smart" } }, path)
              end,
              bufnr = entry.bufnr,
              filename = bufname,
            }
          end,
        },
      },
    },
  },
}
