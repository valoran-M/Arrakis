let get_interval imm i j =
  let (<<) = Int32.shift_left in
  let (>>) = Int32.shift_right_logical in
  let (&&) = Int32.logand in
  let mask = Int32.lognot (-1l << (i - j + 1)) in
  (imm >> j) && mask

