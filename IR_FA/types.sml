structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of (unit -> ty) * unique
          | UNIT
          | BOTTOM
	 (* | NAME of Symbol.symbol * ty option ref *)
	  

end

