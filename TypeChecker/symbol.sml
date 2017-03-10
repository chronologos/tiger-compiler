signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
  val enterscope : 'a table -> 'a table
  val leavescope : 'a table -> 'a table
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)
  
  
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end

  fun name(s,n) = s

  structure Table = IntMapTable(type key = symbol
				fun getInt(s,n) = n)

  type 'a table= 'a Table.table list
  val empty = [Table.empty]
  
  (*  val enter = fn () => (Table.enter) *)
  fun enter(tableList, sym, value) =
    case tableList of [] => (
      print("Illegal use of enter on empty hashtable chain");
      let val newTable = hd empty
      in 
        [Table.enter(newTable, sym, value)]
      end
    )
    | (h::t) =>
      Table.enter(h, sym, value) :: t

  fun look(tableList, sym) =
    case tableList of [] => (
      print("Illegal use of look on empty hashtable chain");
      NONE
    )
    | (h::t) =>
      Table.look(h, sym)
      
  fun enterscope([]) = (
      print("Illegal use of enterscope on empty hashtable chain");
      empty )
    | enterscope(l as h::t) = (h::l)
    
  (*  val look = Table.look *)
  
  fun leavescope(tablelist) = 
    case tablelist of [] => (
      print("Illegal use of exitscope on empty hashtable chain");
      empty )
    | h::t => t
end