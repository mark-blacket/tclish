val set      : Env.t -> string list -> string
val subst    : Env.t -> string list -> string
val proc     : Env.t -> string list -> string
val if_      : Env.t -> string list -> string
val while_   : Env.t -> string list -> string
val for_     : Env.t -> string list -> string
val return   : Env.t -> string list -> string
val break    : Env.t -> string list -> string
val continue : Env.t -> string list -> string
val defaults : Env.t -> unit
