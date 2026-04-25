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

-- Displays signs (like error or warning indicators) in a dedicated column
vim.opt.signcolumn = "yes"

-- Show some certain whitesapce characters
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Always disable code folding.
vim.opt.foldenable = false

-- There's no need to do syntax highlighting past this many columns. The default
-- of 3000 is a bit big and degrades performance.
vim.opt.synmaxcol = 400

-- Hide ~ at the bottom of the buffer
vim.opt.fillchars = "eob: "

-- Highlight the current line
vim.opt.cursorline = true

-- Enable the mouse
vim.opt.mouse = "a"

-- Don't show nvim mode
vim.opt.showmode = false

-- Enable undo/redo changes even after closing and reopening a file
vim.opt.undofile = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 8

-- Tab size of 4 spaces and they are spaces
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- Yes, wrap lines
vim.opt.wrap = true

-- Automatically indents new lines based on syntax
vim.opt.smartindent = true

-- 24-bit RGB color in the terminal

-- if performing an operation that would fail due to unsaved changes in the buffer (like `:q`),
-- instead raise a dialog asking if you wish to save the current file(s)
-- See `:help 'confirm'`
vim.opt.confirm = true

-- Keep a buffer around even when abandoned.
-- Without this, jump-to-definition in LSP clients seems to complain if the
-- file hasn't been saved. In other words, let us go to other buffers even if
-- the current one isn't saved.
vim.opt.hidden = true

vim.opt.autoread = true

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.schedule(function()
	vim.opt.clipboard = "unnamedplus"
end)

-- Enable Nerd font
vim.g.have_nerd_font = true

vim.g.netrw_keepdir = 0

vim.api.nvim_create_autocmd("BufWritePre", {
	callback = function()
		local line = vim.fn.line(".")
		local column = vim.fn.col(".")
		vim.cmd([[%s/\s\+$//e]])
		-- Put the cursor back where it was.
		-- Without this, the `%s` command
		-- will put the cursor after the last
		-- trailing whitespace match.
		vim.fn.cursor(line, column)
	end,
})

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

-- Build fzf-native after install/update.
vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(ev)
		if
			ev.data.spec.name == "telescope-fzf-native.nvim"
			and (ev.data.kind == "install" or ev.data.kind == "update")
		then
			vim.system({ "make" }, { cwd = ev.data.path }):wait()
		end
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
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/NeogitOrg/neogit",
	-- telescope and its dependencies
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/nvim-telescope/telescope-fzf-native.nvim",
	"https://github.com/nvim-telescope/telescope-ui-select.nvim",
	"https://github.com/nvim-telescope/telescope.nvim",
	"https://github.com/saghen/blink.lib",
	"https://github.com/saghen/blink.cmp",
	"https://github.com/folke/snacks.nvim",
})
vim.cmd([[colorscheme tokyonight-night]])

require("which-key")

local cmp = require("blink.cmp")
cmp.build():wait(30000)
cmp.setup()
require("blink.cmp").setup({
	completion = {
		keymap = { preset = "default" },
		sources = { default = { "lsp", "path", "snippets", "buffer" } },
		fuzzy = { implementation = "rust" },
		keyword = { range = "prefix" },
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		ghost_text = { enabled = false },
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
		toml = { "taplo" },
		xml = { "prettier" },
	},
	format_on_save = {
		timeout_ms = 500,
		lsp_format = "fallback",
	},
})

local telescope = require("telescope")
telescope.setup({
	defaults = {
		layout_strategy = "vertical",
		layout_config = {
			height = vim.o.lines, -- maximally available lines
			width = vim.o.columns, -- maximally available columns
			prompt_position = "top",
			preview_height = 0.6, -- 60% of available lines
		},
	},
	extensions = {
		["ui-select"] = { require("telescope.themes").get_dropdown() },
		fzf = {
			fuzzy = true,
			override_generic_sorter = true,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
	},
})
telescope.load_extension("fzf")
telescope.load_extension("ui-select")

local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Telescope find files" })
vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Telescope live grep" })
vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Telescope buffers" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Telescope help tags" })

require("neogit").setup({ disable_hint = true, graph_style = "kitty" })
vim.keymap.set("n", "<space>gg", function()
	require("neogit").open({ kind = "split_below_all" })
end, { desc = "Neogit" })

vim.lsp.enable({
	"lua_ls",
	"rust-analyzer",
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

require("snacks").setup({
	opts = {
		git = true,
		gitbrowse = true,
	},
})
vim.keymap.set("n", "<leader>gh", function()
	require("snacks").gitbrowse()
end, { desc = "Git browse" })
vim.keymap.set("n", "<leader>gB", function()
	require("snacks").git.blame_line()
end, { desc = "Git blame line" })
