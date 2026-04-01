-- Lua syntax file
-- Language:    Sail
-- Maintainer:  Bill McSpadden (bill@riscv.org)
-- Last Update: 
-- Built on verilog.vim from vim63

local vim = vim

-- Clear all syntax items
vim.cmd('syntax clear')

-- Set the local value of the 'iskeyword' option
vim.api.nvim_buf_set_option(0, 'iskeyword', '@,48-57,_,192-255')

-- Syntax definitions
vim.cmd('syntax keyword sailStatement   foreach let val')
vim.cmd('syntax keyword identifier      val function type struct union enum let var if then by')
vim.cmd('syntax keyword identifier      else match in return register ref forall operator effect')
vim.cmd('syntax keyword identifier      overload cast sizeof constant constraint default assert newtype from')
vim.cmd('syntax keyword identifier      pure infixl infixr infix scattered end try catch and to')
vim.cmd('syntax keyword identifier      throw clause as repeat until while do foreach bitfield')
vim.cmd('syntax keyword identifier      mapping where with implicit')

vim.cmd('syntax keyword sailKind        Int Type Order Bool inc dec')
vim.cmd('syntax keyword sailKind        barr depend rreg wreg rmem rmemt wmv wmvt eamem wmem')
vim.cmd('syntax keyword sailKind        exmem undef unspec nondet escape configuration')

vim.cmd('syntax keyword sailType        vector bitvector int nat atom range unit bit real list bool string bits option')
vim.cmd('syntax keyword sailType        uint64_t int64_t bv_t mpz_t')

vim.cmd('syntax keyword sailSpecial     _prove _not_prove create kill convert undefined')

vim.cmd('syntax match sailNumber        "\\<0b[0-1_]\+\\>"')
vim.cmd('syntax match sailNumber        "\\<0x[0-9a-fA-F_]\+\\>"')
vim.cmd('syntax match sailNumber        "\\<[-+]?[0-9]+\\>"')
vim.cmd('syntax match sailNumber        "\\<[-+]?[0-9]+\\(\\.[0-9]*\\|\\)\\>"')

vim.cmd('syntax region sailComment     start="/\\*" end="\\*/" contains=sailTodo')
vim.cmd('syntax match sailComment      "//.*" contains=sailTodo')
vim.cmd('syntax keyword sailTodo       contained TODO FIXME XXX')

vim.cmd('syntax match sailPragma       "$[a-zA-Z0-9_]\\+\\>"')

vim.cmd('syntax region sailString      start=+"+ skip=+\\\\"+ end=+"+ contains=sailEscape')
vim.cmd('syntax match sailEscape       "\\\\\\o\\o\\=\\o\\=" contained')
vim.cmd('syntax match sailEscape       +\\\\[nt"\\\\]+ contained')

vim.cmd('syntax match sailConstant     "\\<[A-Z][A-Z0-9_]\\+\\>"')

vim.cmd('syntax match sailPragma       "$include .*" contains=sailFilename')
vim.cmd('syntax region sailFilename    start=+<+ end=+>+ contained')

-- Highlighting definitions
vim.cmd('hi link sailNumber   Number')
vim.cmd('hi link sailComment  Comment')
vim.cmd('hi link sailGlobal   Define')
vim.cmd('hi link sailPragma   PreProc')
vim.cmd('hi link sailString   String')
vim.cmd('hi link sailConstant Constant')
vim.cmd('hi link sailTodo     Todo')
vim.cmd('hi link sailFilename String')
vim.cmd('hi link sailKind     Type')
vim.cmd('hi link sailSpecial  PreProc')

"" Vim syntax file
"" Language:	Sail
"" Maintainer:	Bill McSpadden (bill@riscv.org)
"" Last Update:  
"" Built on verilog.vim from vim63

"" For version 5.x: Clear all syntax items
"" For version 6.x: Quit when a syntax file was already loaded
"if version < 600
   "syntax clear
"elseif exists("b:current_syntax")
   "finish
"endif

"" Set the local value of the 'iskeyword' option
"if version >= 600
   "setlocal iskeyword=@,48-57,_,192-255
"else
   "set iskeyword=@,48-57,_,192-255
"endif

"" Taken from the "The Sail instruction-set semantics specification language" July 15, 2021
"" Using sail-mode.el as a pattern.
""syn keyword sailStatement   foreach let val
"syntax keyword  identifier  val function type struct union enum let var if then by
"syntax keyword  identifier  else match in return register ref forall operator effect
"syntax keyword  identifier  overload cast sizeof constant constraint default assert newtype from
"syntax keyword  identifier  pure infixl infixr infix scattered end try catch and to
"syntax keyword  identifier  throw clause as repeat until while do foreach bitfield
"syntax keyword  identifier  mapping where with implicit

"syntax keyword  sailKind    Int Type Order Bool inc dec
"syntax keyword  sailKind    barr depend rreg wreg rmem rmemt wmv wmvt eamem wmem
"syntax keyword  sailKind    exmem undef unspec nondet escape configuration

"syntax keyword  sailType    vector bitvector int nat atom range unit bit real list bool string bits option
"syntax keyword  sailType    uint64_t int64_t bv_t mpz_t

"syntax keyword  sailSpecial _prove _not_prove create kill convert undefined

"syntax match    sailNumber      "\<0b[0-1_]\+\>"
"syntax match    sailNumber      "\<0x[0-9a-fA-F_]\+\>"
"syntax match    sailNumber      "\<[-+]\?[0-9]\+\>"
"syntax match    sailNumber      "\<[+-]\=[0-9]\+\(\.[0-9]*\|\)\>"

"syntax region   sailComment     start="/\*" end="\*/" contains=sailTodo
"syntax match    sailComment     "//.*" contains=sailTodo
"syntax keyword  sailTodo        contained TODO FIXME XXX

"syntax match    sailPragma      "$[a-zA-Z0-9_]\+\>"

"syntax region   sailString      start=+"+ skip=+\\"+ end=+"+ contains=sailEscape
"syntax match    sailEscape      +\\[nt"\\]+ contained
"syntax match    sailEscape      "\\\o\o\=\o\=" contained

"syntax match    sailConstant    "\<[A-Z][A-Z0-9_]\+\>"

"syntax match    sailPragma      "$include .*" contains=sailFilename
"syntax region   sailFilename    start=+<+ end=+>+ contained


"if version >= 508 || !exists("did_sail_syn_inits")
   "if version < 508
      "let did_sail_syn_inits = 1
      "command -nargs=+ HiLink hi link <args>
   "else
      "command -nargs=+ HiLink hi def link <args>
   "endif
  "HiLink sailNumber   Number
  "HiLink sailComment  Comment
  "HiLink sailGlobal   Define
  "HiLink sailPragma   PreProc
  "HiLink sailString   String
  "HiLink sailConstant Constant
  "HiLink sailTodo     Todo
  "HiLink sailFilename String
  "HiLink sailKind     Type
  "HiLink sailSpecial  PreProc

"endif




"let b:current_syntax = "sail"

" vim: ts=2
