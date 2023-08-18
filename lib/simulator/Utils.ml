let sign_extended i size =
  Int32.shift_left 
    (Int32.shift_right i (32 - size)) (32 - size) 
