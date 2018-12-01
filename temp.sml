structure Temp: TEMP = struct

type temp = int
type label = Symbol.symbol
		 
val temps = ref 99
val labelCount = ref ~1

structure Table = IntMapTable(type key = int
			      fun getInt n = n)


fun newtemp () = (temps := !temps + 1; !temps)

fun makestring t = "t" ^ Int.toString t

fun newlabel () = (labelCount := !labelCount + 1; Symbol.symbol("L" ^ Int.toString(!labelCount)))

val namedlabel = Symbol.symbol
		      
end
			   
