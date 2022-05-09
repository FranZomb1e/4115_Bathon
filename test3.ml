open Irgen

let _=
  let lexbuf = Lexing.from_channel stdin in
  let program = Bathonparse.program Scanner.tokenize lexbuf in
  let sprogram = Semant.check program in
  print_endline (Irgen.translate sprogram)
