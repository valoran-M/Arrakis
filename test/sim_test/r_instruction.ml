open Simulator

let cpu = Cpu.make 0l
let memory = Memory.make ()

let test () = Alcotest.check Alcotest.int32 "equal" 1l 1l
