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
  > (11:Format_type3:int)
  > (11:Format_type3:int)
  > (11:Format_type3:int)
  > 4:Halt
  > EOF
