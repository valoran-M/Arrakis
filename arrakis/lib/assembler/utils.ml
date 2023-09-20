let get_interval imm i j =
  let open Int32 in
  let (<<) = shift_left in
  let (>>) = shift_right_logical in
  let (&&) = logand in
  let mask = lognot (-1l << (i - j + 1)) in
  (imm >> j) && mask

