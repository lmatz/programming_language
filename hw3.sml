(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** ****)

fun only_capitals(str_list:string list) =
    List.filter(fn a => (Char.isUpper(String.sub(a,0)))) str_list


fun longest_string1 (str_list:string list) =
    List.foldl(fn (x,y) => if String.size(x)<=String.size(y) then y else x) "" str_list


fun longest_string2 (str_list:string list) =
    List.foldl(fn (x,y) => if String.size(x)<String.size(y) then y else x ) "" str_list

fun longest_string_helper f str_list =
    List.foldl(fn (x,y) => if f(String.size(x),String.size(y)) then x else y ) "" str_list

val longest_string3 =
    longest_string_helper(fn (x,y) => x>y ) 

val longest_string4 =
    longest_string_helper(fn (x,y) => x>=y ) 

fun longest_capitalized (str_list:string list) =
    (longest_string1 o only_capitals) (str_list)

fun rev_string(str:string) =
    (String.implode o List.rev o String.explode) str


fun first_answer afb a_list =
    case a_list of
        [] => raise NoAnswer
    | x::xs' =>  case afb x of
                    SOME i => i
                 | NONE => first_answer afb xs'

fun all_answers afb a_list =
    let fun helper accm left_list  =
        case left_list of
            [] => accm
        | x::xs' => case afb x of
                      NONE => raise NoAnswer
                    | SOME y => helper (accm @ y) xs'
    in 
        SOME (helper [] a_list) handle NoAnswer => NONE
    end



fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


fun count_wildcards(pat:pattern) =
    g (fn x => 1) (fn y => 0) pat

fun count_wild_and_variable_lengths (pat : pattern) =
    g (fn x => 1) (fn y => String.size(y)) pat
        
fun count_some_var (x:string * pattern) =
    g (fn a => 0) (fn b => if (#1 x)=b then 1 else 0 ) (#2 x)

fun check_pat(pat:pattern) =
    let fun helper1(pat1:pattern) =
            case pat1 of
                Variable x => [x]
            | TupleP ps => List.foldl( fn (p,i) => (helper1 p)@ i  ) [] ps
            | ConstructorP(_,p) => helper1 p 
            | _ => []
        fun helper2(str_list:string list) =
            case str_list of
                [] => true
                | x::xs' => if List.exists (fn y => x=y) xs'
                       then false
                       else helper2(xs')
    in 
        helper2(helper1(pat))
    end



fun match1 (x:valu*pattern) =
    let val v=(#1 x)
        val p=(#2 x)
    in 
        case p of
            Wildcard => SOME []
        |   Variable s => SOME [(s,v)]
        |   UnitP => (case v of
                        Unit => SOME []
                     |  _ => NONE)
        |   ConstP i => (case v of 
                            Const iv => if i=iv then SOME [] else NONE
                        |   _ => NONE)
        |   TupleP ps => (case v of
                            Tuple vs => if List.length(ps)=List.length(vs)
                                        then (all_answers (fn (a,b)=> match1(a,b)) (ListPair.zip(vs,ps) ))
                                        else NONE        
                        | _ => NONE)
        |   ConstructorP (s1,q) => (case v of 
                                      Constructor(a,b) => if s1=a then match1(b,q) else NONE
                                   | _ => NONE)
    end        


fun first_match  v pat_list =
    SOME  (first_answer (fn (x) => match1(v,x)) pat_list) handle NoAnswer => NONE 









