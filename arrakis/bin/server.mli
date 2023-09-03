val start_server :
  string ->
  Simulator.Arch.t ->
  (string, int32) Hashtbl.t ->
  (int32, 'a * string) Hashtbl.t ->
  unit

