# ian's random thoughts :(
Type checking is hard when you have mutually recursive types.

## CASE 1
```
type r = {B:b, A:a}
type a = b
type b = c
type c = int
```

This is legal. But how do we differentiate from this case:

## CASE 2
```
type r = {B:b, A:a}
type a = b
type b = c
```
This has no base type (assuming c is not defined elsewhere) and is illegal.

So when we declare a record type, the types in the fields can be: 1) in the tenv already, 2) defined within the same TypeDec (mutually recursive) 3) in neither (illegal)

We also need to check for illegal cycles, which are cycles with no record or array type.
## CASE 3
```
type r = {A:a}
type a = b
type b = a
```

On the other hand, this cycle is valid:
## CASE 4
```
type r = {A:a}
type a = b
type b = r
```
we could also have
## CASE 5
```
type r = {A:a}
```
with `a` already in the outer tenv but not in the typedec.

**Idea**: so how bout we add everything in a typedec into a symbol table first.
wait but how do we add things like:
## CASE 6
```
type a = b
type b = int
```

in this case we cannot add 'a' as we do not yet know b's type is int.

**another idea** ok so how about this:
in the function get_fields we:
1. we add all record types to the local_tenv first. record types can be added as Types.RECORD(get_fields, ref() )
2. we then add only Name types in a "double for loop".
in the outer loop, we loop over the entire typedec. everytime we reach a new namety, which is basically symbol * type_symbol, we call a recursive function *R* if it is not an int or string.
33. get_fields will then search local_tenv and tenv when it is called.

- *R(A.Typedec, type_symbol)* looks for what type_symbol maps to in typedec.
- If it finds type_symbol maps to neither an int nor string, it calls itself again. At some point, the recursion will bottom out.
- In this case, either we have a type_symbol that is a int or string OR we cannot find the symbol in the typedec.
- At this point, the recursive function looks at the outer tenv. If the symbol is in the outer tenv, it succeeds else we have something like **CASE 2** where we should rightly return a failure.
- In successful cases we will add the mapping to the local_tenv Let us test this idea on **CASE 7**.

## CASE 7
```
type r = {A:a, RR:rr, C:c}
type rr = {R:r}
type a = b
type b = int
```
Here we assume c was defined to be a string already.

So at the beginning of **Case 7**, the local_tenv looks like:
```
r |-> Types.RECORD (get_fields, ref() ), rr |-> Types.RECORD (get_fields, ref() )
```
the typedec is
```
[{name=r, ty=RecordTy(...)}, {name=r, ty=RecordTy(...)}, {name=a,ty=NameTy(...)}, {name=b,ty=NameTy(...)}]
```
the tenv is:
```
c|->string
```
We now start with the first iteration of the outer loop over the typedec. We find the first NameTy, a|->b. b is not an int or string, so we call `R(typedec, b)`. R finds b|->int. Thus, we put the mapping a|->int into the local_tenv.
local_tenv:
```
r |-> Types.RECORD (get_fields, ref() ), rr |-> Types.RECORD (get_fields, ref() ), a|->int
```
Back to the second iteration of the outer loop, we find the second NameTy b->int. This is added directly to local_tenv.
local_tenv:
```
r |-> Types.RECORD (get_fields, ref() ), rr |-> Types.RECORD (get_fields, ref() ), a|->int, b|->int
```
Then we are done till the get_fields() is called. get_fields has a closure over local_tenv and tenv.
the tenv is:
```
c|->string
```
local_tenv:
```
r |-> Types.RECORD (get_fields, ref() ), rr |-> Types.RECORD (get_fields, ref() ), a|->int, b|->int
```
so if `get_fields()` is called for `r`, it will find `a` in `local_tenv`, `rr` in `local_env` and `c` in `tenv`.

# Okay
So this seems to work, but there are no double for loops in SML. We need to do something like:
```foldl
    (fn(dec,localtenv => let
    val sym = #name dec
    val ty = #ty dec
    fun R = ....
    in
        case ty of NameTy(type_sym, pos) => S.enter(localtenv, sym, R(type_sym))
        | (_) => ???
        )
    end)
    localtenv, typedec
```
