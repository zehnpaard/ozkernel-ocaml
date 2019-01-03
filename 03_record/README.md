03 - Records
=

* Ints are the only numbers (no floats)
* Numbers, procedures and are values
* Builtins called like procedures but do not have procedure values

```
<v> ::=
    <int>
  | <procedure>

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
