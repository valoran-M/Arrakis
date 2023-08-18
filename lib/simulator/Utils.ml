let sign_extended i size =
  Int32.shift_left 
    (Int32.shift_right i (32 - size)) (32 - size) 

let char_to_int32 c = Int32.of_int (Char.code c)
let char_of_int32 c = Char.chr (Int32.to_int c)
