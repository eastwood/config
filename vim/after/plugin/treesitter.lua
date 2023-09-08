local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.fsharp = {
  install_info = {
    url = "~/.config/nvim/grammars/tree-sitter-fsharp",
    files = {"src/scanner.cc", "src/parser.c" },
    generate_requires_npm = false,
    requires_generate_from_grammar = false
  },
  filetype = "fs",
}
