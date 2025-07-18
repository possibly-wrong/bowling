(* Generate all possible games as sequences of frame types. *)
games = Join @@ Outer[Join,
    Tuples[{"strike", "spare", "open"}, 9],

    (* Special case 10th frame strike: followed (or not) by another strike *)
    {{"strike", "strike"}, {"strike", "open"}, {"spare"}, {"open"}},
    1];

(* Minimize number of pins in given game type reaching given score. *)
bowl[game_List, score_Integer] := Module[
  {x, frame, ball, pins, points, solution},
  pins = Join[Table[
     game[[frame]] /. {
       "strike" -> {10},
       "spare" -> {x[frame, 1], 10 - x[frame, 1]},
       "open" -> {x[frame, 1], x[frame, 2]}},
     {frame, 10}],
    game[[10]] /. {
      "strike" :> {
        {game[[11]] /. {"strike" -> 10, "open" -> x[11, 1]}},
        {x[11, 2]}},
      "spare" :> {{x[11, 1]}},
      "open" :> {{}}}];
  points = Sum[
    game[[frame]] /. {
      "strike" :> 10 + Total@Take[pins[[frame + {1, 2}]] // Flatten, 2],
      "spare" :> 10 + Total@Take[pins[[frame + 1]], 1],
      "open" :> x[frame, 1] + x[frame, 2]},
    {frame, 10}];
  solution = Minimize[{
     Total[pins // Flatten],
     And @@ Flatten@{
        points >= score,
        Table[0 <= x[frame, ball], {frame, 11}, {ball, 2}],
        Table[x[frame, 1] + x[frame, 2] <= 9, {frame, 10}],
        x[11, 1] + x[11, 2] <= 10}},
    Array[x, {11, 2}] // Flatten,
    Integers];
  {First[solution], pins /. Last[solution]}]

scores = Map[bowl[#, 100] &, games];
Take[Sort[scores], 3] // Column
