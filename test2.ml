open Sast

let _=
  let lexbuf = Lexing.from_channel stdin in
  let program = Bathonparse.program Scanner.tokenize lexbuf in
  let sprogram = Semant.check program in
  print_endline (string_of_sprogram sprogram)
