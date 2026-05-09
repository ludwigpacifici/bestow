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

vim.pack.add({
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
	sections = {
		lualine_c = {
			{ "filename", path = 3 },
		},
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

vim.diagnostic.config({ virtual_text = true })
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(event)
		vim.lsp.inlay_hint.enable(true, { bufnr = event.buf })

		local function map(lhs, rhs, desc)
			vim.keymap.set("n", lhs, rhs, { buffer = event.buf, desc = desc })
		end

		map("<leader>lDi", function()
			fzf.lsp_workspace_diagnostics()
		end, "Fzf lsp workspace diagnostics")
		map("<leader>lS", function()
			fzf.lsp_workspace_symbols()
		end, "Fzf lsp workspace symbols")
		map("<leader>lca", function()
			fzf.lsp_code_actions()
		end, "Fzf lsp code actions")
		map("<leader>ldc", function()
			fzf.lsp_declarations()
		end, "Fzf lsp declarations")
		map("<leader>ldf", function()
			fzf.lsp_definitions()
		end, "Fzf lsp definitions")
		map("<leader>ldi", function()
			fzf.lsp_document_diagnostics()
		end, "Fzf lsp document diagnostics")
		map("<leader>lf", function()
			fzf.lsp_finder()
		end, "Fzf lsp finder")
		map("<leader>lic", function()
			fzf.lsp_incoming_calls()
		end, "Fzf lsp incoming calls")
		map("<leader>lim", function()
			fzf.lsp_implementations()
		end, "Fzf lsp implementations")
		map("<leader>loc", function()
			fzf.lsp_outgoing_calls()
		end, "Fzf lsp outgoing calls")
		map("<leader>lr", function()
			fzf.lsp_references()
		end, "Fzf lsp references")
		map("<leader>ls", function()
			fzf.lsp_document_symbols()
		end, "Fzf lsp document symbols")
		map("<leader>ltd", function()
			fzf.lsp_typedefs()
		end, "Fzf lsp typedefs")
	end,
})

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
	gitbrowse = { enabled = true },
	lazygit = { enabled = true },
	scratch = { enabled = true },
})

vim.keymap.set("n", "<leader>w", "<cmd>write<cr>", { desc = "Save file" })
vim.keymap.set("n", "<C-s>", "<cmd>write<cr>", { desc = "Save file" })
vim.keymap.set("i", "<C-s>", "<cmd>write<cr>", { desc = "Save file" })

vim.keymap.set("n", "<leader>d", "<cmd>Explore<cr>", { desc = "Explore" })
vim.keymap.set("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete Buffer" })

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
vim.keymap.set("n", "<leader>fl", function()
	fzf.lines()
end, { desc = "Fzf document Grep" })
vim.keymap.set("n", "<leader>fG", function()
	fzf.live_grep()
end, { desc = "Fzf workspace Grep" })
vim.keymap.set("n", "<leader>gb", function()
	fzf.git_blame()
end, { desc = "Fzf git blame" })

vim.keymap.set("n", "<leader>gh", function()
	require("snacks").gitbrowse()
end, { desc = "Git browse" })
vim.keymap.set("n", "<leader>gg", function()
	require("snacks").lazygit()
end, { desc = "Lazygit" })
vim.keymap.set("n", "<leader>.", function()
	require("snacks").scratch()
end, { desc = "Toggle Scratch Buffer" })
