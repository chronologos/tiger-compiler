signature TEMP =
sig
  eqtype temp
  val newtemp : unit -> temp
  val newNamedTemp : string -> temp
  val makestring: temp -> string
  (*val compare : temp * temp -> order*)
  val newNamedTempTrue : string -> temp
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
 (*structure Set : ORD_SET sharing type Set.Key.ord_key = temp
  structure Map : ORD_MAP sharing type Map.Key.ord_key = temp *)
end
