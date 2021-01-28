Sending an invalid command:

  $ ocamlformat-rpc <<EOF
  > foo
  > EOF

Sending an Halt command:

  $ ocamlformat-rpc <<EOF
  > 4:Halt
  > EOF

Sending a Version command with an invalid version:

  $ ocamlformat-rpc <<EOF
  > (7:Version2:v0)
  > EOF
  (Version v1)

Sending a Version command with a valid version:

  $ ocamlformat-rpc <<EOF
  > (7:Version2:v1)
  > EOF
  (Version v1)

Sending several Format_type commands:

  $ ocamlformat-rpc <<EOF
  > (11:Format_type12:type t = int)
  > (11:Format_type16:type t=(int*int))
  > (11:Format_type57:type t = a (* a *) -> b (* b *) -> c (* c *) -> d (* d *))
  > 4:Halt
  > EOF
