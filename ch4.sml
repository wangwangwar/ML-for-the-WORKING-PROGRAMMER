(* 第4章 🌲和具体数据 *)
 
datatype person = King
                | Peer of string * string * int
                | Knight of string
                | Peasant of string;

King;
Peer ("Earl", "Carliesle", 7);
Knight "Gawain";
Peasant "Jack Jones";

val persons = [King, Peasant "Jack Jones", Knight "Gawain"];

fun title King = "His majesty the King"
  | title (Peer (deg, terr, _)) = "The " ^ deg ^ " of " ^ terr
  | title (Knight name) = "Sir " ^ name
  | title (Peasant name) = name;

title(King);
title(Peer ("Earl", "Carliesle", 7));
title(Knight "Gawain");
title(Peasant "Jack Jones");

fun sirs [] = []
  | sirs ((Knight s) :: ps) = s :: (sirs ps)
  | sirs (p :: ps) = sirs ps;

sirs persons;


fun superior (King, Peer _) = true
  | superior (King, Knight _) = true
  | superior (King, Peasant _) = true
  | superior (Peer _, Knight _) = true
  | superior (Peer _, Peasant _) = true
  | superior (Knight _, Peasant _) = true
  | superior _ = false;

superior (King, Peer ("A", "B", 3));
superior (Knight "A", Peasant "S");

(* ex 4.1 *)
fun superior_num (person1, person2) =
  let 
    fun level King = 4
      | level (Peer _) = 3
      | level (Knight _) = 2
      | level (Peasant _) = 1
  in
    (level person1) > (level person2)
  end;

superior_num (King, Peer ("A", "B", 3));
superior_num (Peasant "A", King);
