fun interpreter(input : string, output : string) = 
let
	val outStream = TextIO.openOut output
	datatype thing = 
	name of string 
	| str of string
	| boollit of bool
	| num of int
	| errorlit of string
	| myunit of (string*thing)
	| closure of (string*string list*thing list*thing list*bool*string)
	fun readlist (infile : string) = 
		let
  			val ins = TextIO.openIn infile
  			fun loop ins =
   				case TextIO.inputLine ins of
      			SOME line => line :: loop ins
    			| NONE      => []
		in
  			loop ins before TextIO.closeIn ins
		end
	val inputList = readlist(input)
	fun isNum someString = List.all Char.isDigit (String.explode someString)
	fun isName somestring = List.all Char.isAlphaNum (String.explode somestring)
	(*mainfunction*)
fun makestack (input : string list, ret : thing list list, bindings : thing list list) =
	    case input of
	      [] => hd ret
	      | (x::xs) => (print(x); (case x of
	        "add\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => (case y of
	          			num(cy) => makestack(xs, (num(cx+cy)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (num(cx+numval)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => (case y of
	       								num(cy) => makestack(xs, (num(numval+cy)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (num(numval+numvaly)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))

	        |"sub\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => (case y of
	          			num(cy) => makestack(xs, (num(cy-cx)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (num(numval-cx)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => (case y of
	       								num(cy) => makestack(xs, (num(cy-numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (num(numvaly-numval)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
	        |"mul\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => (case y of
	          			num(cy) => makestack(xs, (num(cx*cy)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (num(cx*numval)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => (case y of
	       								num(cy) => makestack(xs, (num(numval*cy)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (num(numval*numvaly)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
	        |"div\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => if cx = 0 then makestack(xs, (errorlit(":error:")::h)::thll, bindings) 
	          				else (case y of
	          			num(cy) => makestack(xs, (num(cy div cx)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (num(numval div cx)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => if numval = 0 then makestack(xs, (errorlit(":error:")::h)::thll, bindings)
	       								 else (case y of
	       								num(cy) => makestack(xs, (num(cy div numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (num(numvaly div numval)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
	        |"rem\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => if cx = 0 then makestack(xs, (errorlit(":error:")::h)::thll, bindings) 
	          				else (case y of
	          			num(cy) => makestack(xs, (num(cy mod cx)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (num(numval mod cx)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => if numval = 0 then makestack(xs, (errorlit(":error:")::h)::thll, bindings)
	       								 else (case y of
	       								num(cy) => makestack(xs, (num(cy mod numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (num(numvaly mod numval)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
	        |"neg\n" => (case ret of
	        	h::thll => (case h of
	        		x::y => (case x of
	        			num(c) => makestack(xs, (num(~c)::y)::thll, bindings)
	        			|name(nameval) => 
		        			let
		        				fun findname x = 
	       							(case x of 
	       								myunit(nameis, valis) => if nameis = nameval then true else false
	       						|		_ => false)
		        			in
		        				(case (List.find findname (hd bindings)) of
		        					SOME(found) => (case found of
		        						myunit(nameis, valis) => (case valis of
			        						num(numval) => makestack(xs, (num(~numval)::y)::thll, bindings)
			        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        					| NONE => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        			end
	        			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
					| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	        	| _ => makestack(x::xs, []::ret, bindings))
	        |"pop\n" => (case ret of
	        	h::thll => (case h of
	        	(x::y) => makestack(xs, y::thll, bindings)
	        	|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	        	| _ => makestack(x::xs, []::ret, bindings))
	        |":true:\n" => (case ret of
	        	h::thll => makestack(xs, (boollit(true)::h)::thll,bindings)
	        	| _ => makestack(x::xs, []::ret, bindings))
	        |":false:\n" => (case ret of
	        	h::thll => makestack(xs, (boollit(false)::h)::thll,bindings)
	        	| _ => makestack(x::xs, []::ret, bindings))
	        |":error:\n" => (case ret of
	        	h::thll => makestack(xs, (errorlit(":error:")::h)::thll,bindings)
	        	| _ => makestack(x::xs, []::ret, bindings))
	       	|"swap\n" => (case ret of
	       		h::thll => (case h of
	       		x::y::rest => makestack(xs, (y::x::rest)::thll, bindings)
	       		| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       		| _ => makestack(x::xs, []::ret, bindings))
	       	|"and\n" => (case ret of
	       		h::thll => (case h of
		       		x::y::rest => (case x of
		       			boollit(cx) => (case y of
		       				boollit(cy) => makestack(xs, (boollit(cx andalso cy)::rest)::thll, bindings)
		       				| name(nameval) => 
		        			let
		        				fun findname x = 
	       							(case x of 
	       								myunit(nameis, valis) => if nameis = nameval then true else false
	       						|		_ => false)
		        			in
		        				(case (List.find findname (hd bindings)) of
		        					SOME(found) => (case found of
		        						myunit(nameis, valis) => (case valis of
			        						boollit(numval) => makestack(xs, (boollit(numval andalso cx)::rest)::thll, bindings)
			        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        					| NONE => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        			end
		       				| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       			| name(nameval) =>
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							boollit(numval) => (case y of
	       								boollit(cy) => makestack(xs, (boollit(cy andalso numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							boollit(numvaly) => (makestack(xs, (boollit(numval andalso numvaly)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
		       			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       		| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       		| _ => makestack(x::xs, []::ret, bindings))
	       	|"or\n" => (case ret of
	       		h::thll => (case h of
		       		x::y::rest => (case x of
		       			boollit(cx) => (case y of
		       				boollit(cy) => makestack(xs, (boollit(cx orelse cy)::rest)::thll, bindings)
		       				| name(nameval) => 
		        			let
		        				fun findname x = 
	       							(case x of 
	       								myunit(nameis, valis) => if nameis = nameval then true else false
	       						|		_ => false)
		        			in
		        				(case (List.find findname (hd bindings)) of
		        					SOME(found) => (case found of
		        						myunit(nameis, valis) => (case valis of
			        						boollit(numval) => makestack(xs, (boollit(numval orelse cx)::rest)::thll, bindings)
			        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        					| NONE => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        			end
		       				| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       			| name(nameval) =>
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							boollit(numval) => (case y of
	       								boollit(cy) => makestack(xs, (boollit(cy orelse numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							boollit(numvaly) => (makestack(xs, (boollit(numval orelse numvaly)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
		       			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       		| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       		| _ => makestack(x::xs, []::ret, bindings))
	       	|"not\n" => (case ret of
	        	h::thll => (case h of
	        		x::y => (case x of
	        			boollit(c) => makestack(xs, (boollit(not c)::y)::thll, bindings)
	        			|name(nameval) => 
		        			let
		        				fun findname x = 
	       							(case x of 
	       								myunit(nameis, valis) => if nameis = nameval then true else false
	       						|		_ => false)
		        			in
		        				(case (List.find findname (hd bindings)) of
		        					SOME(found) => (case found of
		        						myunit(nameis, valis) => (case valis of
			        						boollit(numval) => makestack(xs, (boollit(not numval)::y)::thll, bindings)
			        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        					| NONE => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		        			end
	        			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
					| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	        	| _ => makestack(x::xs, []::ret, bindings))
	       	|"equal\n" =>  (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => (case y of
	          			num(cy) => makestack(xs, (boollit(cx=cy)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (boollit(cx=numval)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x = 
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => (case y of
	       								num(cy) => makestack(xs, (boollit(numval=cy)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (boollit(numval=numvaly)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
	       	|"lessThan\n" => (case ret of
	          h::thll => (case h of
	          	x::y::rest => (case x of
	          		num(cx) => (case y of
	          			num(cy) => makestack(xs, (boollit(cy < cx)::rest)::thll, bindings)
	          			|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (boollit(numval < cx)::rest)::thll, bindings))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end

	          			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          		| name(nameval) => 
      					let
	       					fun findname x =
	       					(case x of 
	       						myunit(nameis, valis) => if nameis = nameval then true else false
	       						|_ => false)
	       				in
	       					(case (List.find findname (hd bindings)) of
	       					SOME(found) => (case found of
	       						myunit(nameis, valis) => (case valis of
	       							num(numval) => (case y of
	       								num(cy) => makestack(xs, (boollit(cy < numval)::rest)::thll, bindings)
	       								| name(nameval) => 
						       				let
						       					fun findname x = 
						       					(case x of 
						       						myunit(nameis, valis) => if nameis = nameval then true else false
						       						|_ => false)
						       				in
						       					(case (List.find findname (hd bindings)) of
						       					SOME(found) => (case found of
						       						myunit(nameis, valis) => (case valis of
						       							num(numvaly) => (makestack(xs, (boollit(numvaly < numval)::rest)::thll, bindings))
						       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
						       				end
	       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
	       				end
	          		|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          	|  _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	          | _ => makestack(x::xs, []::ret, bindings))
			|"if\n" => (case ret of
				h::thll => (case h of
					x::y::z::rest => (case z of
						boollit(boolval) => if boolval then makestack(xs, (x::rest)::thll, bindings) else makestack(xs, (y::rest)::thll, bindings)
						|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							boollit(numvaly) => if numvaly then makestack(xs, (x::rest)::thll, bindings) else makestack(xs, (y::rest)::thll, bindings)
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => (makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
			       				end
						| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
					| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
				| _ => makestack(x::xs, []::ret, bindings))
	       	|"bind\n" => (case ret of
	       		h::thll => (case h of
		       		value::nam::rest => (case bindings of
		       			hbind::tbind => (case nam of
			       			name(n) => (case value of
			       				num(numval) => (makestack(xs, (myunit(n, num(numval))::rest)::thll,(myunit(n,num(numval))::(hbind))::tbind))
			       				|boollit(boolval) => (makestack(xs, (myunit(n, boollit(boolval))::rest)::thll, (myunit(n,boollit(boolval))::(hbind))::tbind))
			       				|str(strval) => (makestack(xs, (myunit(n, str(strval))::rest)::thll, (myunit(n,str(strval))::(hbind))::tbind))
			       				|myunit(unitn, unitval) => (makestack(xs, ((myunit(n, myunit(unitn, unitval)))::rest)::thll, (myunit(n, myunit(unitn, unitval))::hbind)::tbind))
			       				|name(nameval) => 
			       				let
			       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
			       				in
			       					(case (List.find findname (hd bindings)) of
			       					SOME(found) => (case found of
			       						myunit(nameis, valis) => (case valis of
			       							num(numval) => (makestack(xs, (myunit(n, num(numval))::rest)::thll,(myunit(n,num(numval))::(hbind))::tbind))
						       				|boollit(boolval) => (makestack(xs, (myunit(n, boollit(boolval))::rest)::thll, (myunit(n,boollit(boolval))::(hbind))::tbind))
						       				|str(strval) => (makestack(xs, (myunit(n, str(strval))::rest)::thll, (myunit(n,str(strval))::(hbind))::tbind))
						       				|myunit(unitn, unitval) => (makestack(xs, ((myunit(n, myunit(unitn, unitval)))::rest)::thll, (myunit(n, myunit(unitn, unitval))::hbind)::tbind))
			       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       						|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       					|NONE => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
			       				end
			       				| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       				| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       			| [] => makestack(x::xs, ret, []::bindings))
			       	| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       		| _ => makestack(x::xs, []::ret, bindings))
			|"let\n" =>	makestack(xs, []::ret, (hd bindings)::bindings)
			|"end\n" =>
				(case ret of
					h::thll => (case bindings of
						hbind::tbind => (case h of
							t::rest => (case thll of
								h2::t2 => makestack(xs, (t::h2)::t2, tbind)
								| [] => makestack(x::xs, []::ret, bindings))
							| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						| [] => makestack(x::xs, ret, []::bindings))
					| [] => makestack(x::xs, []::ret, bindings))
	       	|"quit\n" => (hd ret)
	       	|"return\n" => (case ret of
					h::thll => (case bindings of
						hbind::tbind => (case h of
							t::rest => (case thll of
								h2::t2 => (case t of
									name(nameval) =>
										let
											fun findname x = 
			       								(case x of 
			       									myunit(nameis, valis) => if nameis = nameval then true else false
			       									|_ => false)
			       							fun findfun x = 
												(case x of 
													myunit(nameis, valis) => if nameis = "thisisit" then true else false
													|_ => false)	
										in
											(case List.find findname (hd bindings) of
												SOME(found) => (case found of
													myunit(unitnx, unitval) => 
											       			(case List.find findfun (hd bindings) of
											       				SOME(foundfun) => (case foundfun of
											       					myunit(unitn, unitvalx) => (case unitvalx of
											       						closure(paramname, funcommandlist, newstack, funbindings, true, argname) => 
											       							let
											       								fun findname x = 
																					(case x of
																						myunit(nameis, valis) => if nameis = paramname then true else false
																						|_ => false)
											       							in
											       								(case (List.find findname (hd bindings)) of
											       									SOME(foundx) => (case foundx of
											       										myunit(nameis, valis) =>(case unitval of
											       											closure(a,b,c,d,e,f) => (makestack(tl xs, (name(unitnx)::h2)::t2, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings))))
											       											| _ => (makestack(tl xs, (unitval::h2)::t2, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings)))))
											       										| _ => makestack(tl xs, (tl ret), (tl bindings)))
											       									| NONE => makestack(tl xs, (tl ret), (tl bindings)))
											       							end
											       						| closure(paramname, funcommandlist, newstack, funbindings, false, argname) => 
											       							 let
											       								fun findname x = 
																					(case x of
																						myunit(nameis, valis) => if nameis = paramname then true else false
																						|_ => false)
											       							in
											       								(case (List.find findname (hd bindings)) of
											       									SOME(foundx) => (case foundx of
											       										myunit(nameis, valis) => (case unitval of
											       											closure(a,b,c,d,e,f) => (makestack(tl xs, (name(unitnx)::h2)::t2, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings))))
											       											| _ => (makestack(tl xs, (unitval::h2)::t2, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings)))))
											       										| _ => makestack(tl xs, (tl ret), (tl bindings)))
											       									| NONE => makestack(tl xs, (tl ret), (tl bindings)))
											       							end
											       						| _ => (makestack(tl xs, (unitval::h2)::t2, (tl bindings))))
											       					| _ => makestack(tl xs, (tl ret), (tl bindings)))
											       				| NONE => makestack(tl xs, (tl ret), (tl bindings)))

													| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
												| NONE => makestack(tl xs, (t::h2)::t2, tbind))
										end
									| _ => let
								       			fun findfun x = 
													(case x of 
														myunit(nameis, valis) => if nameis = "thisisit" then true else false
														|_ => false)
								       		in
								       			(case List.find findfun (hd bindings) of
											       				SOME(foundfun) => (case foundfun of
											       					myunit(unitn, unitvalx) => (case unitvalx of
											       						closure(paramname, funcommandlist, newstack, funbindings, true, argname) => 
											       							let
											       								fun findname x = 
																					(case x of
																						myunit(nameis, valis) => if nameis = paramname then true else false
																						|_ => false)
											       							in
											       								(case (List.find findname (hd bindings)) of
											       									SOME(foundx) => (case foundx of
											       										myunit(nameis, valis) =>(makestack(tl xs, (t::h2)::t2, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings))))
											       										| _ => makestack(tl xs, (tl ret), (tl bindings)))
											       									| NONE => makestack(tl xs, (tl ret), (tl bindings)))
											       							end
											       						| _ => makestack(tl xs, (t::h2)::t2, (tl bindings)))
											       					| _ => makestack(tl xs, (tl ret), (tl bindings)))
											       				| NONE => makestack(tl xs, (tl ret), (tl bindings)))
								       		end)
								| [] => makestack(x::xs, []::ret, bindings))
							| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
						| [] => makestack(x::xs, ret, []::bindings))
					| [] => makestack(x::xs, []::ret, bindings))
	       	|"funEnd\n" => 
	       		let
	       			fun findfun x = 
						(case x of 
							myunit(nameis, valis) => if nameis = "thisisit" then true else false
							|_ => false)
	       		in
	       			(case List.find findfun (hd bindings) of
				       				SOME(foundfun) => (case foundfun of
				       					myunit(unitn, unitvalx) => (case unitvalx of
				       						closure(paramname, funcommandlist, newstack, funbindings, true, argname) => 
				       							let
				       								fun findname x = 
														(case x of
															myunit(nameis, valis) => if nameis = paramname then true else false
															|_ => false)
				       							in
				       								(case (List.find findname (hd bindings)) of
				       									SOME(foundx) => (case foundx of
				       										myunit(nameis, valis) =>(makestack(xs, tl ret, (myunit(argname, valis)::(hd (tl bindings)))::(tl (tl bindings))))
				       										| _ => makestack(xs, (tl ret), (tl bindings)))
				       									| NONE => makestack(xs, (tl ret), (tl bindings)))
				       							end
				       						| _ => makestack(xs, (tl ret), (tl bindings)))
				       					| _ => makestack(xs, (tl ret), (tl bindings)))
				       				| NONE => makestack(xs, (tl ret), (tl bindings)))
	       		end
	       	|"call\n" => (case ret of
	       		h::thll => (case h of
	       			(x::y::rest) => (case x of
	       				name(nameval) => 
		       				let
		       					fun findname x = 
			       					(case x of 
			       						myunit(nameis, valis) => if nameis = nameval then true else false
			       						|_ => false)
		       				in
		       					(case List.find findname (hd bindings) of
		       						SOME(found) => (case found of
		       							myunit(nameis, valis) => (case valis of
		       								closure(param, funcommandlist, newstack, funbindings, isinout, argname) => 
		       									(case y of
		       										num(numval) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, num(numval))::myunit("thisisit", valis)::funbindings)::bindings)
		       										|str(strval) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, str(strval))::myunit("thisisit", valis)::funbindings)::bindings)
		       										|boollit(boolval) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, boollit(boolval))::myunit("thisisit", valis)::funbindings)::bindings) 
		       										|errorlit(errorval) => makestack(xs, (errorlit(":error:")::h)::thll, bindings)
		       										|myunit(unitn, unitval) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, myunit(unitn, unitval))::myunit("thisisit", valis)::funbindings)::bindings)
		       										|name(namevalxy) =>
			       										let
			       											fun findnamex xx = 
										       					(case xx of
										       						myunit(nameisx, valisx) => if nameisx = namevalxy then true else false
										       						|_ => false)
			       										in
															(case List.find findnamex (hd bindings) of
																SOME(foundx) => (case foundx of
																	myunit(nameisx, valisx) => (case valisx of
																		 num(numvalx) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, num(numvalx))::myunit("thisisit", closure(param, funcommandlist, newstack, funbindings, isinout, namevalxy))::funbindings)::bindings)
							       										|str(strvalx) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, str(strvalx))::myunit("thisisit", closure(param, funcommandlist, newstack, funbindings, isinout, namevalxy))::funbindings)::bindings)
							       										|boollit(boolvalx) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, boollit(boolvalx))::myunit("thisisit", closure(param, funcommandlist, newstack, funbindings, isinout, namevalxy))::funbindings)::bindings)
							       										|closure(paramx, funcommandlistx, newstackx, funbindingsx, isinoutx, argname) => (makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, closure(paramx, funcommandlistx, newstackx, funbindingsx, isinoutx, namevalxy))::myunit("thisisit", closure(param, funcommandlist, newstack, funbindings, isinout, namevalxy))::funbindings)::bindings)) 
							       										|myunit(unitnx, unitvalx) => makestack(funcommandlist@xs, newstack::(rest::thll), (found::myunit(param, myunit(unitnx, unitvalx))::myunit("thisisit", closure(param, funcommandlist, newstack, funbindings, isinout, namevalxy))::funbindings)::bindings)
							       										| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))

																	| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
																|NONE => makestack(funcommandlist@xs, newstack::(rest::thll), (myunit(param, name(nameval))::funbindings)::bindings))
			       										end
			       									| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       								| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       							|_ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
		       						|NONE => (print("notfound\n");makestack(xs, (errorlit(":error:")::h)::thll, bindings)))
		       				end
	       				| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       			| _ => makestack(xs, (errorlit(":error:")::h)::thll, bindings))
	       		| _ => makestack(x::xs, []::ret, bindings))
	        |a => 
        	 (case ret of
        		h::thll =>

	        	if String.substring(a,0,5) = "push " then
	              let
	                val rest = substring(a,5,size(a)-6)
	              in
	              	(*String*)
	                if String.isPrefix "\"" rest then
	                  let
	                    val st = String.substring(rest,0,size(rest))
	                    val go = String.substring(st, 1, size(st)-2)

	                  in
	                  	makestack(xs, (str(go)::h)::thll, bindings)
	                  end
	                (*positive num*)
	                else if isNum rest then
	                  let
	                    val SOME x = Int.fromString rest;
	                  in
	                     makestack(xs, (num(x)::h)::thll,bindings)
	                  end
	                (*negative num*)
	                else if String.isPrefix "-" rest then 
	                  let
	                    val negnum = String.substring(rest,1,size(rest) - 1)
	                  in
	                    if isNum negnum then 
	                      let
	                        val SOME x = Int.fromString rest
	                      in
	                     	 makestack(xs, (num(x)::h)::thll,bindings)
	                      end
	                    else makestack(xs, (errorlit(":error:")::h)::thll,bindings)
	                  end
	                (*name*)
	           		else if Char.isAlpha (String.sub(rest,0)) andalso isName rest then 
	           			makestack(xs, (name(rest)::h)::thll,bindings)
	           		(*other*)
	           		else makestack(xs, (errorlit(":error:")::h)::thll,bindings)
	              end


	              (*function*)
	          else if String.substring(a,0,4) = "fun " then
	          	let
	          		val rest = substring(a,4,size(a)-5)
	          		val funandparam = String.tokens (fn x => x = #" ") rest
	          		val funname = hd funandparam
	          		val param = hd (tl funandparam)
	          		fun makefunbody (lxs : string list, retl : string list, numoffuns : int list list, rest : string list) = 
	          			(case lxs of
	          				y::ys => if y = "if\n" then makefunbody(ys, retl@[y], numoffuns, ys)
	          							else if String.substring(y,0,4) = "fun " then makefunbody(ys, retl@[y], []::numoffuns, ys)
	          							else if y = "funEnd\n" then if (length numoffuns) = 0 then (retl@[y], ys)
	          									else makefunbody(ys, retl@[y], (tl numoffuns), ys)
	          							else makefunbody(ys, retl@[y], numoffuns, ys)
	          				| [] => ([],rest))
	          		val funcommands = makefunbody(xs, [], [], xs)

	          	in
	          		case funcommands of
	          			(xs : string list, ys : string list) => 
	          					makestack(ys, (myunit(funname, str(":unit:"))::hd ret)::tl ret, (myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, false, ""))::hd bindings, false, ""))::hd bindings, false, ""))::hd bindings)::tl bindings)
	          	end

	           else if String.substring(a,0,9) = "inOutFun " then
	          	let
	          		val rest = substring(a,9,size(a)-10)
	          		val funandparam = String.tokens (fn x => x = #" ") rest
	          		val funname = hd funandparam
	          		val param = hd (tl funandparam)
	          		fun makefunbody (lxs : string list, retl : string list, numoffuns : int list list, rest : string list) = 
	          			(case lxs of
	          				y::ys => if y = "if\n" then makefunbody(ys, retl@[y], numoffuns, ys)
	          							else if String.substring(y,0,4) = "fun " then makefunbody(ys, retl@[y], []::numoffuns, ys)
	          							else if y = "funEnd\n" then if (length numoffuns) = 0 then (retl@[y], ys)
	          									else makefunbody(ys, retl@[y], (tl numoffuns), ys)
	          							else makefunbody(ys, retl@[y], numoffuns, ys)
	          				| [] => ([],rest))
	          		val funcommands = makefunbody(xs, [], [], xs)

	          	in
	          		(case funcommands of
	          			(xs : string list, ys : string list) => 
	          					makestack(ys, (myunit(funname, str(":unit:"))::hd ret)::tl ret, (myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], myunit(funname, closure(param, xs, [], hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings, true, ""))::hd bindings,true, ""))::hd bindings,true, ""))::hd bindings)::tl bindings))
	          	end


	          else makestack(xs, (errorlit(":error:")::h)::thll,bindings)
	          | _ => makestack(x::xs, []::ret, bindings))))

	val outStack = makestack(inputList, [[]], [[]])

	fun printOut ([]) = (TextIO.closeOut outStream)
		| printOut (x::xs) = (case x of
			name(c) => (TextIO.output(outStream, c^"\n"); printOut(xs))
			|myunit(c, d) => (TextIO.output(outStream, ":unit:"^"\n"); printOut(xs))
			|str(c) => (TextIO.output(outStream, c^"\n"); printOut(xs))
			|boollit(c) => (TextIO.output(outStream, (":"^Bool.toString c^":"^"\n")); printOut(xs))
			|num(c) => if Int.sign c = ~1 then (TextIO.output(outStream, "-"^Int.toString(~c)^"\n"); printOut(xs))
				else (TextIO.output(outStream, Int.toString(c)^"\n"); printOut(xs))
			|errorlit(c) => (TextIO.output(outStream, c^"\n"); printOut(xs))
			|closure(c) => (TextIO.output(outStream, "dontmatter\n"); printOut(xs)))
in
	printOut(outStack)
end

(*val _ = interpreter("input.txt", "output.txt")*)
