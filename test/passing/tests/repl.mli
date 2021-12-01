type t = k
(** Blocks such as this one:
    {[
      let x = 2 in x + x
    ]}
    Or, as of odoc 2.1, this one:
    {@ocaml[
      let x = 2
    ]}*)

val x : t
(** A toplevel block:
    {[
    # let x = 2 and y = 3 in x+y;;
    ]} *)

val x : t
(** A toplevel block:
    {[
    # let x = 2;;
    val x : int = 2
    ]} *)

val x : t
(** A toplevel block:
    {[
    # let x = 2;;
    val x : int = 2
    # x + 2;;
    - : int = 4
    # let x = 2 and y = 3 in x+y;;
    ]} *)
