fun is_older(x:int*int*int, y:int*int*int) =
    if (#1 x) < (#1 y)
    then true
    else if (#1 x) > (#1 y)
    then false
    else 
        if (#2 x) < (#2 y)
        then true
        else if (#2 x) > (#2 y)
        then false
        else 
            if (#3 x) < (#3 y)
            then true
            else false


fun number_in_month(x:(int*int*int) list,y:int) =
    if null x
    then 0
    else
        if (#2 (hd x))=y
        then 1+number_in_month((tl x),y)
        else 0+number_in_month((tl x),y)



fun number_in_months(x:(int*int*int) list,y:int list) =
    if null x
    then 0
    else 
        if null y
        then 0
        else number_in_month(x,(hd y)) + number_in_months(x,(tl y)) 

fun dates_in_month(x:(int*int*int) list, y:int) =
    if null x
    then []
    else 
        if (#2 (hd x))=y
        then (hd x)::dates_in_month(tl x,y)
        else dates_in_month(tl x,y)

fun dates_in_months(x:(int*int*int) list, y:int list) =
    if null x
    then []
    else 
        if null y
        then []
        else  dates_in_month(x,(hd y))@dates_in_months(x,(tl y))
        

fun get_nth(x:string list,y:int) =
    if y=1
    then (hd x)
    else get_nth( (tl x),y-1 )
        
fun date_to_string(x:int*int*int) =
    let val mon=["January","February","March","April","May","June","July","August","September","October","November","December"] 
        in get_nth(mon,(#2 x))^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
    end

fun number_before_reaching_sum(x:int, y:int list) =
    if hd y>=x
    then 0
    else 1+number_before_reaching_sum(x-(hd y),(tl y))


fun what_month(x:int) =
    let val mon=[31,28,31,20,31,30,31,31,30,31,30,31]
        in 1+number_before_reaching_sum(x,mon)
    end

fun month_range(x:int,y:int) =
    if x<=y
    then what_month(x)::month_range(x+1,y)
    else []

fun oldest(x:(int*int*int) list) =
    if null x
    then NONE
    else let fun helper (y: (int*int*int) list)=
                 if null (tl y)
                 then hd y
                 else let val future=helper(tl y)
                      in 
                        if is_older(hd y,future)
                        then hd y
                        else future
                      end
         in 
            SOME (helper(x))
         end


(*helper function:remove duplicate*)
fun remove_duplicates(x:int list) =
    if null x
    then []
    else 
        let fun find_same(a:int,y:int list) =
            if null y
            then false
            else 
                if a=(hd y)
                then true
                else find_same(a,(tl y))
        in 
            if find_same((hd x),(tl x))
            then remove_duplicates((tl x))
            else (hd x)::remove_duplicates((tl x))
        end



fun number_in_months_challenge(x:(int*int*int) list,y:int list) =
    number_in_months(x,remove_duplicates(y))
    


fun dates_in_months_challenge(x:(int*int*int) list,y:int list) =
    dates_in_months(x,remove_duplicates(y))


fun get_days_in_month(month:int,year:int) =
    if month = 2 andalso (year mod 400 = 0 orelse year mod 4 = 0 andalso not (year mod 100 = 0))
    then 29
    else 
        let val month_sums = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            fun helper(cur_month: int, months: int list) =
                if cur_month=1
                then hd months
                else helper(cur_month-1,(tl months))
        in 
             helper(month, month_sums)
        end



fun resonable_date(date:int*int*int) =
    let val day = #3 date; val month = #2 date; val year = #1 date in 
        if year>0 andalso month>0 andalso month<12 andalso day>0 andalso day<32
        then 
            if get_days_in_month(month,year)<day
            then false
            else true
        else false
    end
    
    

