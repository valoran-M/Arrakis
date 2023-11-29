type state =
  { 
    channel : Format.formatter;
    history : Simulator.History.t;
    line_debug : (int, int32)    Hashtbl.t;
    add_debug  : (int, int32)    Hashtbl.t;
    label      : (string, int32) Hashtbl.t;
  }

