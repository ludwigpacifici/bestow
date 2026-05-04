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

-- Sync clipboard between OS and Neovim because it can increase startup-time.
vim.api.nvim_create_autocmd("UIEnter", {
	once = true,
	callback = function()
		vim.opt.clipboard = "unnamedplus"
	end,
})

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
vim.keymap.set("i", "<C-s>", "<cmd>write<cr>", { desc = "Save file" })

vim.keymap.set("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete Buffer" })

vim.pack.add({
	"https://github.com/NeogitOrg/neogit",
	"https://github.com/folke/snacks.nvim",
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/folke/which-key.nvim",
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/saghen/blink.cmp",
	"https://github.com/saghen/blink.lib",
	"https://github.com/stevearc/conform.nvim",
})
vim.cmd.colorscheme("tokyonight-night")

require("which-key").setup({})

require("nvim-treesitter").setup({
	install_dir = vim.fn.stdpath("data") .. "/site",
})
require("nvim-treesitter")
	.install({
		"bash",
		"c",
		"cpp",
		"diff",
		"dockerfile",
		"editorconfig",
		"git_config",
		"gitcommit",
		"gitignore",
		"java",
		"javascript",
		"json",
		"make",
		"mermaid",
		"rust",
		"terraform",
		"typescript",
		"yaml",
		"zig",
	})
	:wait(300000)

local fzf = require("fzf-lua")
fzf.setup({
	"ivy",
	grep = { hidden = true },
})
vim.keymap.set("n", "<leader>ff", function()
	fzf.files()
end, { desc = "Fzf files" })
vim.keymap.set("n", "<leader>fb", function()
	fzf.buffers()
end, { desc = "Fzf buffer" })
vim.keymap.set("n", "<leader>fg", function()
	fzf.grep_curbuf()
end, { desc = "Fzf document Grep" })
vim.keymap.set("n", "<leader>fG", function()
	fzf.grep()
end, { desc = "Fzf workspace Grep" })

vim.keymap.set("n", "<leader>lDi", function()
	fzf.lsp_workspace_diagnostics()
end, { desc = "Fzf lsp workspace diagnostics" })
vim.keymap.set("n", "<leader>lS", function()
	fzf.lsp_workspace_symbols()
end, { desc = "Fzf lsp workspace symbols" })
vim.keymap.set("n", "<leader>lca", function()
	fzf.lsp_code_actions()
end, { desc = "Fzf lsp code actions" })
vim.keymap.set("n", "<leader>ldc", function()
	fzf.lsp_declarations()
end, { desc = "Fzf lsp declarations" })
vim.keymap.set("n", "<leader>ldf", function()
	fzf.lsp_definitions()
end, { desc = "Fzf lsp definitions" })
vim.keymap.set("n", "<leader>ldi", function()
	fzf.lsp_document_diagnostics()
end, { desc = "Fzf lsp document diagnostics" })
vim.keymap.set("n", "<leader>lf", function()
	fzf.lsp_finder()
end, { desc = "Fzf lsp finder" })
vim.keymap.set("n", "<leader>lic", function()
	fzf.lsp_incoming_calls()
end, { desc = "Fzf lsp incoming calls" })
vim.keymap.set("n", "<leader>lim", function()
	fzf.lsp_implementations()
end, { desc = "Fzf lsp implementations" })
vim.keymap.set("n", "<leader>loc", function()
	fzf.lsp_outgoing_calls()
end, { desc = "Fzf lsp outgoing calls" })
vim.keymap.set("n", "<leader>lr", function()
	fzf.lsp_references()
end, { desc = "Fzf lsp references" })
vim.keymap.set("n", "<leader>ls", function()
	fzf.lsp_document_symbols()
end, { desc = "Fzf lsp document symbols" })
vim.keymap.set("n", "<leader>ltd", function()
	fzf.lsp_typedefs()
end, { desc = "Fzf lsp typedefs" })

local cmp = require("blink.cmp")
cmp.build():wait(60000)
cmp.setup({
	completion = {
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		keyword = { range = "prefix" },
	},
	-- fuzzy = { implementation = "rust" },
	keymap = { preset = "default" },
	signature = { enabled = true },
	sources = { default = { "lsp", "path", "snippets", "buffer" } },
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
		java = { "google-java-format" },
		lua = { "stylua" },
		markdown = { "dprint" },
		rust = { "rustfmt" },
		sh = { "shfmt" },
		toml = { "taplo" },
		xml = { "prettier" },
	},
	format_on_save = {
		timeout_ms = 2000,
		lsp_format = "fallback",
	},
})

require("neogit").setup({ disable_hint = true, graph_style = "kitty" })
vim.keymap.set("n", "<space>gg", function()
	require("neogit").open({ kind = "split_below_all" })
end, { desc = "Neogit" })

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

vim.lsp.enable({
	"lua_ls",
	"rust-analyzer",
	"bashls",
})

require("snacks").setup({
	bigfile = { enabled = true },
	git = { enabled = true },
	gitbrowse = { enabled = true },
	scratch = { enabled = true },
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
