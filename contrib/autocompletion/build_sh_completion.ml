let spec            = Options.spec
let executable_name = "arrakis"

let () =
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  Array.iter (Argsh.Builder.file executable_name spec) argv

