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

local AutoSaveGroup = vim.api.nvim_create_augroup("AutoSave", { clear = true })
vim.api.nvim_create_autocmd("FocusLost", {
	group = AutoSaveGroup,
	callback = function()
		vim.cmd.wall({ mods = { silent = true } })
	end,
})
vim.api.nvim_create_autocmd({ "BufLeave", "WinLeave" }, {
	group = AutoSaveGroup,
	callback = function()
		if vim.bo.modified and vim.bo.modifiable and not vim.bo.readonly and vim.bo.buftype == "" then
			vim.cmd.update({ mods = { silent = true } })
		end
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

vim.pack.add({
	"https://github.com/folke/snacks.nvim",
	"https://github.com/folke/which-key.nvim",
	"https://github.com/rebelot/kanagawa.nvim",
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/saghen/blink.cmp",
	"https://github.com/saghen/blink.lib",
	"https://github.com/stevearc/conform.nvim",
})

require("kanagawa").setup({
	dimInactive = true,
	theme = "dragon",
	background = {
		dark = "wave",
		light = "dragon",
	},
})
vim.cmd.colorscheme("kanagawa-dragon")

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
		"python",
		"rust",
		"terraform",
		"typescript",
		"yaml",
		"zig",
	})
	:wait(300000)
vim.api.nvim_create_autocmd("FileType", {
	pattern = {
		"bash",
		"c",
		"cpp",
		"diff",
		"dockerfile",
		"editorconfig",
		"gitconfig",
		"gitcommit",
		"gitignore",
		"java",
		"javascript",
		"json",
		"make",
		"mermaid",
		"python",
		"rust",
		"terraform",
		"typescript",
		"yaml",
		"zig",
	},
	callback = function()
		pcall(vim.treesitter.start)
	end,
})

local fzf = require("fzf-lua")
fzf.setup({
	"ivy",
	grep = { hidden = true },
})

require("blink.cmp").build():wait(60000)
require("blink.cmp").setup({
	completion = {
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		keyword = { range = "prefix" },
	},
	-- fuzzy = { implementation = "rust" },
	keymap = {
		preset = "default",
		["<CR>"] = { "accept", "fallback" },
	},
	signature = { enabled = true },
	sources = { default = { "lsp", "path", "snippets", "buffer" } },
})

require("lualine").setup({
	options = {
		component_separators = { left = "", right = "" },
		section_separators = { left = "", right = "" },
	},
	sections = {
		lualine_c = {
			{ "filename", path = 3 },
		},
		lualine_x = {
			{
				"lsp_status",
				symbols = { spinner = { "⧗" } },
			},
			"encoding",
			"fileformat",
			"filetype",
		},
	},
})

require("conform").setup({
	formatters_by_ft = {
		java = { "google-java-format" },
		lua = { "stylua" },
		markdown = { "dprint" },
		python = { "ruff_organize_imports", "ruff_format" },
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

vim.diagnostic.config({ virtual_text = true })
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(event)
		vim.lsp.inlay_hint.enable(true, { bufnr = event.buf })

		local function map(lhs, rhs, desc)
			vim.keymap.set("n", lhs, rhs, { buffer = event.buf, desc = desc })
		end

		map("<leader>lI", function()
			fzf.lsp_workspace_diagnostics()
		end, "Fzf lsp workspace diagnostics")
		map("<leader>lS", function()
			fzf.lsp_workspace_symbols()
		end, "Fzf lsp workspace symbols")
		map("<leader>la", function()
			fzf.lsp_code_actions()
		end, "Fzf lsp code actions")
		map("<leader>lc", function()
			fzf.lsp_declarations()
		end, "Fzf lsp declarations")
		map("<leader>lf", function()
			fzf.lsp_definitions()
		end, "Fzf lsp definitions")
		map("<leader>li", function()
			fzf.lsp_document_diagnostics()
		end, "Fzf lsp document diagnostics")
		map("<leader>lm", function()
			fzf.lsp_implementations()
		end, "Fzf lsp implementations")
		map("<leader>lr", function()
			fzf.lsp_references()
		end, "Fzf lsp references")
		map("<leader>ls", function()
			fzf.lsp_document_symbols()
		end, "Fzf lsp document symbols")
		map("<leader>lt", function()
			fzf.lsp_typedefs()
		end, "Fzf lsp typedefs")
	end,
})

vim.lsp.config("lua_ls", {
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim" },
			},
		},
	},
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "rust",
	callback = function()
		vim.cmd("compiler cargo")
	end,
})

vim.lsp.enable({
	"lua_ls",
	"rust_analyzer",
	"bashls",
	"jdtls",
	"basedpyright",
	"ruff",
})

require("snacks").setup({
	bigfile = { enabled = true },
	gitbrowse = { enabled = true },
	lazygit = { enabled = true },
	scratch = { enabled = true },
})

local function cd_git_root()
	local root = vim.fs.root(0, ".git")

	if not root then
		vim.notify("No git root found from current buffer", vim.log.levels.WARN)
		return
	end

	vim.api.nvim_set_current_dir(root)
	vim.notify("cwd: " .. root)
end

vim.api.nvim_create_user_command("GitRoot", cd_git_root, {
	desc = "Set cwd to git root of current buffer",
})

vim.keymap.set("n", "<leader>w", "<cmd>write<cr>", { desc = "Save file" })
vim.keymap.set("n", "<leader>q", "<cmd>quit<cr>", { desc = "Quit" })
vim.keymap.set("n", "<leader>d", "<cmd>Explore<cr>", { desc = "Explore" })
vim.keymap.set("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete Buffer" })

vim.keymap.set("n", "<leader>gr", cd_git_root, { desc = "Set cwd to git root" })

vim.keymap.set("n", "<leader>k", function()
	fzf.keymaps()
end, { desc = "Fzf keymap" })
vim.keymap.set("n", "<leader>ff", function()
	fzf.files()
end, { desc = "Fzf files" })
vim.keymap.set("n", "<leader>fh", function()
	fzf.history()
end, { desc = "Fzf old files" })
vim.keymap.set("n", "<leader>fb", function()
	fzf.buffers()
end, { desc = "Fzf buffer" })
vim.keymap.set("n", "<leader>fg", function()
	fzf.lines()
end, { desc = "Fzf document Grep" })
vim.keymap.set("n", "<leader>fG", function()
	fzf.live_grep()
end, { desc = "Fzf workspace Grep" })
vim.keymap.set("n", "<leader>gb", function()
	fzf.git_blame()
end, { desc = "Fzf git blame" })
vim.keymap.set("n", "<leader>p", function()
	fzf.registers()
end, { desc = "Fzf registers" })

vim.keymap.set("n", "<leader>gh", function()
	require("snacks").gitbrowse()
end, { desc = "Git browse" })
vim.keymap.set("n", "<leader>gg", function()
	require("snacks").lazygit()
end, { desc = "Lazygit" })
vim.keymap.set("n", "<leader>.", function()
	require("snacks").scratch()
end, { desc = "Toggle Scratch Buffer" })
