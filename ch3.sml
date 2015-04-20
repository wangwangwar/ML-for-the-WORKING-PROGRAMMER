(** 3.2 **)

nil;
9::[];
5::[9];
3::[5,9];

[Math.sin 0.5, Math.cos 0.5, Math.exp 0.5];

fun upto (m, n) =
  if m>n then [] else m::upto(m+1, n);

upto(2, 5);


(** 3.2 **)

fun prod [] = 1
  | prod (n::ns) = n * (prod ns);

prod [2, 3, 5];

fun maxl [m] : int = m
  | maxl (m::n::ns) = if m > n 
                      then maxl (m::ns)
                      else maxl (n::ns);
maxl [~4, 0, ~12];
(* maxl []; *)

fun factl n = prod (upto (1, n));
factl 7;

explode "Banquo";
implode it;


(** 3.3 **)

fun null [] = true
  | null (_::_) = false;
null [];
null [3];

fun hd (x::_) = x;
hd [[[1, 2], [3]], [[4]]];
hd it;
hd it;

fun tl (_::xs) = xs;
tl ["Out", "damned", "spot!"];
tl it;
tl it;

fun prod ns = if null ns 
                      then 1
                      else (hd ns) * (prod (tl ns));
prod [2, 3, 3];

(* ex 3.1 *)

local
  fun max (m, ns) =
    if ns = []
    then m
    else if m > (hd ns) then max (m, (tl ns)) else max ((hd ns), (tl ns))
in
  fun maxl2 l = max ((hd l), (tl l))
end;

maxl2 [1, 3, 5];

(* ex 3.2 *)

fun last l =
  if (tl l) = []
  then (hd l)
  else last (tl l);

last [1, 2];


(** 3.4 **)

fun nlength [] = 0
  | nlength (x::xs) = 1 + nlength xs;

nlength [[1, 2, 3], [4, 5, 6]];

local
  fun addlen (n, []) = n
    | addlen (n, x::l) = addlen (n+1, l)
in
  fun length l = addlen (0, l)
end;

length [1, 2, 3];
length [];

fun take ([], i) = []
  | take (x::xs, i) = if i>0 then x::take(xs, i-1)
                      else [];

take (explode "Throw physic to the dogs!", 5);

fun rtake ([], _, taken) = taken
  | rtake (x::xs, i, taken) =
  if i>0 
  then rtake (xs, i-1, x::taken)
  else taken;

rtake ([1, 2, 3, 4, 5], 3, []);

fun drop ([], _) = []
  | drop (x::xs, i) =
  if i>0 then drop (xs, i-1)
  else x::xs;

drop (explode "I'm king of the world!", 10);

(* ex 3.3 *)

take ([1, 2, 3], 5);
take ([1, 2, 3], ~1);

(* ex 3.4 *)

fun nth (x::xs, n) =
  if n>0 then nth (xs, n-1)
  else if n=0 then x else raise Subscript;

nth ([1, 2, 3], 2);

(* 3.5 *)

infixr 5 @;
fun ([] @ ys) = ys
  | ((x::xs) @ ys) = x :: (xs@ys);
  
["Why", "sinks"] @ ["that", "cauldron?"];
[[2, 4, 6, 8], [3, 9]] @ [[5], [7]];

fun nrev [] = [] 
  | nrev (x::xs) = (nrev xs) @ [x];

fun revAppend ([], ys) = ys
  | revAppend (x::xs, ys) = revAppend (xs, x::ys);

revAppend (["Macbeth", "and", "Banquo"], ["all", "hail!"]);

fun rev xs = revAppend (xs, []);

rev [1, 2, 3, 4];

(* test 3.5 *)

fun ([] @ ys) = ys
  | (xs @ []) = xs
  | ((x::xs) @ ys) = x :: (xs@ys);

(* test 3.6 *)

fun nrevTest [] = []
  | nrevTest (x::xs) = (nrevTest xs) @ x;

(* fail => nrevTest [1, 2, 3, 4]; *)
nrevTest [[1, 2], [3, 4]];

(* test 3.7
*
* nrev [1, 2, 3, 4]
*   (nrev [2, 3, 4]) @ [1]
*   ((nrev [3, 4]) @ [2]) @ [1]
*   (((nrev [4]) @ [3]) @ [2]) @ [1]
*   ((((nrev []) @ [4]) @ [3]) @ [2]) @ [1]
*   ((([] @ [4]) @ [3]) @ [2]) @ [1]
*   (([4] @ [3]) @ [2]) @ [1]
*   ([4, 3] @ [2]) @ [1]
*   [4, 3, 2] @ [1]
*   [4, 3, 2, 1]
*
* rev [1, 2, 3, 4]
*   revAppend ([1, 2, 3, 4], [])
*   revAppend ([2, 3, 4], 1::[])
*   revAppend ([3, 4], 2::(1::[]))
*   revAppend ([4], 3::(2::(1::[])))
*   revAppend ([], 4::(3::(2::(1::[]))))
*   4::(3::(2::(1::[])))
*   4::(3::(2::[1]))
*   4::(3::[2, 1])
*   4::[3, 2, 1]
*   [4, 3, 2, 1]
*
*)


(* 3.6 *)

fun concat [] = []
  | concat (l::ls) = l @ concat ls;

concat [["When", "shall"], ["we", "three"], ["meet", "again"]];

fun zip(x::xs, y::ys) = (x, y) :: zip(xs, ys)
  | zip _ = [];

zip ([1, 2, 3], [2, 3, 4]);
zip ([1, 2, 3], []);

fun conspair ((x, y), (xs, ys)) = (x::xs, y::ys);
fun unzip [] = ([], [])
  | unzip (pair::pairs) = conspair (pair, unzip pairs);
unzip [(1, 2), (3, 4), (5, 6)];

fun unzip2 [] = ([], [])
  | unzip2 ((x, y)::pairs) =
      let val (xs, ys) = unzip pairs
      in (x::xs, y::ys) end;

unzip2 [(1, 2), (3, 4), (5, 6)];

fun rev_unzip ([], xs, ys) = (xs, ys)
  | rev_unzip ((x, y)::pairs, xs, ys) =
      rev_unzip (pairs, x::xs, y::ys);

(* test 3.8 *)

fun f [] = []
  | f ([]::ls) = f (ls)
  | f ((x::l)::ls) = x::f(l::ls);

f [[1, 2], [2, 3]];
(*
* concat [[1, 2], [2, 3]]
* [1, 2] @ concat [[2, 3]]
* 1::([2] @ concat [[2, 3]])
* 1::(2::([] @ concat [[2, 3]]))
* 1::(2::(concat [[2, 3]]))
* 1::(2::([2, 3] @ (concat [])))
* 1::(2::([2, 3] @ []))
* 1::(2::(2::([3] @ [])))
* 1::(2::(2::(3::([] @ []))))
* 1::(2::(2::(3::[])))
* 1::(2::(2::[3]))
* 1::(2::[2, 3])
* 1::[2, 2, 3]
* [1, 2, 2, 3]
*
* f [[1, 2], [2, 3]]
* 1::f([2]::[[2, 3]])
* 1::(2::f([]::[[2, 3]]))
* 1::(2::f([[2, 3]]))
* 1::(2::(2::f([3]::[])))
* 1::(2::(2::(3::f([]::[]))))
* 1::(2::(2::(3::f([]))))
* 1::(2::(2::(3::[])))
* 1::(2::(2::[3]))
* 1::(2::[2, 3])
* 1::[2, 2, 3]
* [1, 2, 2, 3]
*)

(* 3.7 *)

fun change (coinvals, 0) = []
  | change (c::coinvals, amount) =
      if amount < c then change (coinvals, amount)
      else c::change(c::coinvals, amount-c);

val gb_coins = [50, 20, 10, 5, 2, 1]
and us_coins = [25, 10, 5, 1];
change (gb_coins, 43);
change (us_coins, 43);
(* change ([5, 2], 16); *)

fun allChange (coins, coinvals, 0) = [coins]
  | allChange (coins, [], amount) = []
  | allChange (coins, c::coinvals, amount) =
      if amount < 0 then []
      else allChange (c::coins, c::coinvals, amount-c) @ 
           allChange (coins, coinvals, amount);

allChange ([], [5, 2], 16);
allChange ([], gb_coins, 16);


(* test 3.11 *)

local 
val romanList = ["M",   "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
val valueList = [1000,  900,  500, 400,  100, 90,   50,  40,   10,  9,    5,   4,    1]
fun numberToRoman2 (0, roman, _, _) = [roman]
  | numberToRoman2 (_, roman, [], _) = []
  | numberToRoman2 (_, roman, _, []) = []
  | numberToRoman2 (amount, roman, r::romanList, v::valueList) =
      if amount < v then numberToRoman2 (amount, roman, romanList, valueList)
      else numberToRoman2 (amount-v, roman ^ r, r::romanList, v::valueList)
in
  fun numberToRoman amount = numberToRoman2 (amount, "", romanList, valueList)
end;

numberToRoman 1984;


(* Test 3.13 *)

fun allChange2 (coins, coinvals, 0) = [coins]
  | allChange2 (coins, [], amount) = []
  | allChange2 (coins, c::coinvals, amount) =
      if amount < 0 then []
      else allChange2 (c::coins, coinvals, amount-c) @ 
           allChange2 (coins, coinvals, amount);

allChange2 ([], [10, 10, 5, 2, 1], 16);
allChange2 ([], [5, 5, 2, 1], 4);

(* Test 3.14 *)

fun allChange3 (coins, all, coinvals, 0) = coins::all
  | allChange3 (coins, all, [], amount) = all
  | allChange3 (coins, all, c::coinvals, amount) =
      if amount < 0 then all
      else allChange3 (c::coins, 
                      allChange3 (coins, all, coinvals, amount),
                      c::coinvals, 
                      amount-c);

allChange3 ([], [], [10, 5, 2, 1], 10);


(* 3.8 *)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1-p)::bincarry(p, ps);

bincarry (0, [1, 1]);

bincarry (1, [1, 1]);
(* (1-1)::bincarry(1, [1])
* 0::((1-1)::bincarry(1::[]))
* 0::(0::(1::[]))
* [0, 0, 1]
*)

bincarry (1, []);

bincarry (1, [1, 0]);
(* (1-1)::bincarry(1, [0])
* 0::(1::bincarry(0, []))
* 0::(1::[])
* [0, 1]
*)

fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) =
      ((c+p+q) mod 2) :: binsum ((c+p+q) div 2, ps, qs);

binsum (0, [1, 1, 0, 1], [0, 1, 1, 1, 1]);

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod (ps, qs)
  | binprod (1::ps, qs) = binsum (0, qs, 0::binprod (ps, qs));

binprod ([1, 1, 0, 1], [0, 1, 1, 1, 1]);
