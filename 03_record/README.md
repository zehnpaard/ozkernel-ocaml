03 - Records
=

* Ints are the only numbers (no floats)
* Numbers, procedures and records are values
* Builtins called like procedures but do not have procedure values
* Booleans are represented as records

```
<v> ::=
    <int>
  | <procedure>
  | <record>

<record> ::=
    <literal>
  | <literal> (<feature>1: <x>1 .. <feature>n: <x>n )

<literal> ::= <atom> | <bool>
<feature> ::= <atom> | <bool> | <int>

<bool> ::= true | false

(* <x> starts with uppercase, <atom> starts with lowercase *)

<procedure> ::=
    proc { $ <x>1 .. <x>n } <s> end
 
<builtin> ::=
    "+"
  | "-"
  | "*"
  | "div"
  | "mod"
  | "Browse"

<s> ::=
    skip
  | <s>1, <s>2
  | local <x> in <s> end
  | <x>1 = <x>2
  | <x> = <v>
  | { <builtin> <x> ... }
```
