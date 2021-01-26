Sending an invalid command:

  $ ocamlformat-rpc <<EOF
  > foo
  > EOF

Sending an Halt command:

  $ ocamlformat-rpc <<EOF
  > 4:Halt
  > EOF

Sending several Format_type commands:

  $ ocamlformat-rpc <<EOF
  > (11:Format_type12:type t = int)
  > (11:Format_type16:type t=(int*int))
  > (11:Format_type57:type t = a (* a *) -> b (* b *) -> c (* c *) -> d (* d *))
  > 4:Halt
  > EOF
  (Format_type"type t = int\n")
