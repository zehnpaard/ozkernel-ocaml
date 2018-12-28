01 - Minimal
=

* Ints are the only numbers (no floats)
* Numbers are the only values
* No records
* No procedures

<v> = <int>

<s> ::=
    skip
  | <s>1, <s>2
  | local <x> in <s> end
  | <x>1 = <x>2
  | <x> = <v>
