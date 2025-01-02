type t

val create : unit -> t
val create_inner : t -> t
val reset : t -> unit
val return : t -> string
val set_var : t -> string -> string -> unit
val set_proc : t -> string -> int -> int -> (t -> string list -> string) -> unit
val var : t -> string -> string
val proc : t -> string -> string list -> unit
