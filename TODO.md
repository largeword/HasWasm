TODO
----
- [ ] traverse & build implicitly called functions / global var
- [ ] imports
- [ ] change the evaluation #2 to "is the syntax similar enough to wasm text syntax?"
- [x] add more i32 / f32 instructions
- [x] declare global var, set & get

instrs:
t.const
t.unop
t.binop
t.testop
t.relop
block
loop
if-else
br
br_if
return
call
t.set_local
t.get_local
t.set_global
t.get_global
func
global
module
