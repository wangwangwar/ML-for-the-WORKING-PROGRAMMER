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
