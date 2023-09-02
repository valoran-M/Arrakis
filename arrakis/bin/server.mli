type server

val loop : server -> unit
val start_server : string -> server
val close_server : server -> unit
