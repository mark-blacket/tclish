exception Return of string
exception Break
exception Continue

val eval : Env.t -> string -> string
val subst : Env.t -> string -> string
val split : string -> string list
