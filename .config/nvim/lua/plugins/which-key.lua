return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function ()
    vim.opt.timeout = true
    vim.opt.timeoutlen = 300
  end,
  config = function ()
    require("which-key").setup()
    require("which-key").register {
      ["<leader>f"] = { name = "[F]ile", _ = "which_key_ignore" },
      ["<leader>b"] = { name = "[B]uffer", _ = "which_key_ignore" },
      ["<leader>h"] = { name = "[H]elp", _ = "which_key_ignore" },
      ["<leader>g"] = { name = "[G]it", _ = "which_key_ignore" },
      ["<leader>l"] = { name = "[L]SP", _ = "which_key_ignore" },
      ["<leader>p"] = { name = "[P]ackages", _ = "which_key_ignore" },
    }
  end,
}
