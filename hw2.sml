(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun all_except_option (x:string, y: string list) =
    let fun helper(a:string, b:string list) =
        case b of
            [] => []
        |   s::xs'=> if same_string(a,s)
                     then helper(a,xs')
                     else s::helper(a,xs')
        val new_list=helper(x,y)
    in 
        if new_list=y 
        then NONE
        else SOME new_list
    end



fun get_substitutions1 (x:string list list, s:string) =
    case x of
        [] => []
    | a::b' => case all_except_option(s,a) of
                   NONE => get_substitutions1(b',s)
               |   SOME thing => thing @ get_substitutions1(b',s)


fun get_substitutions2 (x:string list list, s:string) =
    let fun helpers(a:string list list, c:string list) =
        case a of
            [] => c
        | d::f' => case all_except_option(s,d) of
                      NONE => helpers(f',c)
                   |  SOME thing => helpers(f', c @ thing)
    in
        helpers(x,[])
    end


fun similar_names(x:string list list, {first=first,middle=middle,last=last}) =
    let val first_name_list=first::get_substitutions2(x,first);
        fun helpern (str:string list) =
            case str of
                [] => []
            | x::xs' => {first=x,middle=middle,last=last}::helpern(xs')
    in
        helpern(first_name_list)
    end


fun card_color (x,_) =
    case x of
        Clubs => Black
    |   Spades => Black
    |   Hearts => Red
    |   Diamonds => Red

fun card_value (_,x) =
    case x of 
        Jack => 10
    |   Queen => 10
    |   King => 10
    |   Ace => 11
    |   Num y => y


fun remove_card (cards:card list, c:card, e) =
    let fun helper (cs:card list) =
        case cs of
            [] => []
        | x::xs' => if x=c
                    then xs'
                    else x::helper(xs')
        val left_cards= helper(cards)
    in
        if left_cards=cards 
        then raise e
        else left_cards
    end

fun all_same_color(cards:card list) =
    case cards of
        [] => true
    |  x::[] => true
    |  x::y::rest => if card_color(x)=card_color(y)
                   then all_same_color(y::rest)
                   else false

fun sum_cards(cards:card list) =
    let fun sum_left_cards(left_cards:card list,sum:int)=
        case left_cards of
            [] => sum
        | x::y' => sum_left_cards(y',sum+card_value(x))
    in
        sum_left_cards(cards,0)
    end

fun score (held_cards:card list, goal:int) =
    let val sum = sum_cards(held_cards)
        val prelim_score= if sum>goal
                          then 3 * (sum-goal)
                          else (goal-sum)
        val same_color=all_same_color(held_cards)
    in 
        if same_color 
        then prelim_score div 2 
        else prelim_score
    end


fun officiate(card_list:card list, move_list:move list,goal:int) =
    let fun helper(held_cards:card list, left_move_list:move list, left_cards:card list) =
        case left_move_list of
            [] => score(held_cards,goal)
        | x::xs' => case x of
                       Discard card=> helper(remove_card(held_cards,card,IllegalMove),xs',left_cards)
                   |   Draw => case left_cards of
                                  [] => score(held_cards,goal)
                                  | y::ys' => let val new_held_cards=y::held_cards 
                                              in 
                                                 if sum_cards(new_held_cards)>goal 
                                                 then score(new_held_cards,goal)
                                                 else helper(new_held_cards,xs',ys')
                                              end
    in 
        helper([],move_list,card_list)
    end
