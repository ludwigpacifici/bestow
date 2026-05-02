require("vim._core.ui2").enable()

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.termguicolors = true
vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.signcolumn = "yes"
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.foldenable = false
vim.opt.synmaxcol = 400
vim.opt.fillchars = "eob: "
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.showmode = false
vim.opt.undofile = true
vim.opt.scrolloff = 8
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.wrap = true
vim.opt.smartindent = true
vim.opt.confirm = true
vim.opt.hidden = true
vim.opt.autoread = true

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.schedule(function()
	vim.opt.clipboard = "unnamedplus"
end)

vim.g.have_nerd_font = true
vim.g.netrw_keepdir = 0

-- Force reload of files that change on disk outside of vim.
-- See also `autoread` in `lua/options.lua`.
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "BufWinEnter", "CursorHold", "CursorHoldI" }, {
	callback = function()
		if vim.fn.mode() ~= "c" then
			vim.cmd.checktime()
		end
	end,
})
-- Emits a warning if the file changed.
vim.api.nvim_create_autocmd("FileChangedShellPost", {
	callback = function()
		vim.cmd.echohl("WarningMsg")
		vim.cmd.echo([["File changed on disk. Buffer reloaded."]])
		vim.cmd.echohl("None")
	end,
})

-- Current line hightlighted only for active split
local ActiveWindowCursorlineGroup = vim.api.nvim_create_augroup("ActiveWindowCursorline", { clear = true })
vim.api.nvim_create_autocmd("WinEnter", {
	group = ActiveWindowCursorlineGroup,
	callback = function()
		vim.wo.cursorline = true
	end,
})
vim.api.nvim_create_autocmd("WinLeave", {
	group = ActiveWindowCursorlineGroup,
	callback = function()
		vim.wo.cursorline = false
	end,
})

vim.cmd("packadd nvim.difftool")

vim.keymap.set("n", "<leader>w", "<cmd>write<cr>", { desc = "Save file" })
vim.keymap.set("n", "<C-s>", "<cmd>write<cr>", { desc = "Save file" })
vim.keymap.set("i", "<C-s>", "<esc><cmd>write<cr>gui", { desc = "Save file" })

vim.keymap.set("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Save file" })

vim.pack.add({
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/folke/which-key.nvim",
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/NeogitOrg/neogit",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/saghen/blink.lib",
	"https://github.com/saghen/blink.cmp",
	"https://github.com/folke/snacks.nvim",
})
vim.cmd.colorscheme("tokyonight-night")

require("which-key")

require("fzf-lua").setup({
	"ivy",
})
vim.keymap.set("n", "<leader>ff", function()
	require("fzf-lua").files()
end, { desc = "Fzf files" })
vim.keymap.set("n", "<leader>fb", function()
	require("fzf-lua").buffers()
end, { desc = "Fzf buffer" })
vim.keymap.set("n", "<leader>fs", function()
	require("fzf-lua").lsp_document_symbols()
end, { desc = "Fzf document Symbols" })
vim.keymap.set("n", "<leader>fS", function()
	require("fzf-lua").lsp_worksapce_symbols()
end, { desc = "Fzf workspace Symbols" })
vim.keymap.set("n", "<leader>fd", function()
	require("fzf-lua").diagnostics_document()
end, { desc = "Fzf document Diagnostics" })
vim.keymap.set("n", "<leader>fD", function()
	require("fzf-lua").diagnostics_workspace()
end, { desc = "Fzf workspace Diagnostics" })

local cmp = require("blink.cmp")
cmp.build():wait(30000)
cmp.setup()
require("blink.cmp").setup({
	completion = {
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		fuzzy = { implementation = "rust" },
		keymap = { preset = "default" },
		keyword = { range = "prefix" },
		signature = { enabled = true },
		sources = { default = { "lsp", "path", "snippets", "buffer" } },
	},
})

require("lualine").setup({
	options = {
		theme = "tokyonight",
		component_separators = { left = "", right = "" },
		section_separators = { left = "", right = "" },
	},
})

require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		markdown = { "dprint" },
		rust = { "rustfmt" },
		sh = { "shfmt" },
		toml = { "taplo" },
		xml = { "prettier" },
	},
	format_on_save = {
		timeout_ms = 500,
		lsp_format = "fallback",
	},
})

require("neogit").setup({ disable_hint = true, graph_style = "kitty" })
vim.keymap.set("n", "<space>gg", function()
	require("neogit").open({ kind = "split_below_all" })
end, { desc = "Neogit" })

vim.lsp.enable({
	"lua_ls",
	"rust-analyzer",
	"bashls",
})
vim.diagnostic.config({ virtual_text = true })

vim.lsp.config["lua_ls"] = {
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim" },
			},
		},
	},
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	root_markers = { ".luarc.json", ".git" },
}

vim.lsp.config["rust-analyzer"] = {
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_markers = { "Cargo.toml" },
}

vim.lsp.config.bashls = {
	cmd = { "bash-language-server", "start" },
	filetypes = { "bash", "sh" },
}

require("snacks").setup({
	opts = {
		bigfile = true,
		git = true,
		gitbrowse = true,
		scratch = true,
	},
})
vim.keymap.set("n", "<leader>gh", function()
	require("snacks").gitbrowse()
end, { desc = "Git browse" })
vim.keymap.set("n", "<leader>gB", function()
	require("snacks").git.blame_line()
end, { desc = "Git blame line" })
vim.keymap.set("n", "<leader>.", function()
	require("snacks").scratch()
end, { desc = "Toggle Scratch Buffer" })
