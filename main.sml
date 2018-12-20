structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   (*structure R = RegAlloc *)

   (* fun getsome (SOME x) = x *)
	   

 fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ F.name frame ^ "\n")
	 (*val _ = Printtree.printtree(out,body);*)
	 val stms = Canon.linearize body
	 val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms;
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	 val bodyInstrs = List.concat(map (Mipsgen.codegen frame) stms')

	 val (instrs, maxArgs) = F.procEntryExit2(frame, bodyInstrs)
	 val (newInstrs, allocation) = RegAlloc.alloc(instrs, frame)
	 val refinedInstrs = Mipsgen.removeRedundantMoves(newInstrs, allocation)

	 val {prolog, body, epilog} = F.procEntryExit3(frame, refinedInstrs, maxArgs)
	 (* t104, t105, t106 ton tai dong thoi, check call codegen*)
         val format0 = Assem.format (F.makestring F.tempMap) (*F.makestring allocation*) 
     in
	 TextIO.output(out, prolog);
	 app (fn i => TextIO.output(out,format0 i)) newInstrs;
	 TextIO.output(out, epilog)
     end
   | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun writeHeader out =
    TextIO.output(out, ".text\n"^"main:\n")

fun withOpenFile fname f = 
    let
	val out = TextIO.openOut fname
	val _ = writeHeader out
    in (f out before TextIO.closeOut out) 
       (*handle e => (TextIO.closeOut out; raise Fail "open file") *)
    end


fun compile filename = 
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn);
	(* val _ = (PrintAbsyn.print (TextIO.stdOut, absyn); 
	 print "\n-------------------\n") *)
    in 
        withOpenFile (filename ^ ".s") 
		     (fn out => (app (emitproc out) frags));
	Translate.clearFrags()
    end

end



