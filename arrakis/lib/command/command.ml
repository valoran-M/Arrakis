module type Command = sig
  val help : unit -> string

  val exec : string list -> unit
end
