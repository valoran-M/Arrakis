module type Command = sig
  type ret

  val description : string
  val help        : unit -> string

  val exec : string list -> ret
end

