open Util
open Util.Board

let copy b1 b2 i j = copy b1 b2 0 i j

let shapes = 
  let zs () = [| 0; 0; 0; 0|] in
  [| of_matrix
       [|[| 1; 1; 1; 1|]; zs(); zs(); zs()|];
     of_matrix
       [|[| 1; 1; 1; 0|];
         [| 1; 0; 0; 0|]; zs(); zs()|];
     of_matrix
       [|[| 1; 1; 1; 0|];
         [| 0; 0; 1; 0|]; zs(); zs()|];
     of_matrix
       [|[| 1; 1; 0; 0|];
         [| 1; 1; 0; 0|]; zs(); zs()|];
     of_matrix
       [|[| 1; 1; 0; 0|];
         [| 0; 1; 1; 0|]; zs(); zs()|];
     of_matrix 
       [|[| 0; 1; 1; 0|];
         [| 1; 1; 0; 0|]; zs(); zs()|];
     of_matrix
       [|[| 0; 1; 0; 0|];
         [| 1; 1; 1; 0|]; zs(); zs()|];
  |]

let new_shape sp () =
  let id = Array.length shapes |> Random.int in
  let current = shapes.(id) |> dup in
  update (fun x -> x * (id+1)) current;
  { shape=current; currentX=sp; currentY=0 }

let freeze board {shape=cur; currentX=curX; currentY=curY} =
  copy cur board curY curX

let clear_lines board =
  let w, h = width board, height board in
  let line_is_full i = for_all_row (( <> ) 0) i board in
  let rec loop i =
    if i < 0 then () 
    else (if line_is_full i 
          then
            let sound = get_by_id "clearsound" in play sound;
            for j = i downto 1 do
              set_row j board (get_row (j-1) board);
            done;
            set_row 0 board (Array.make w 0);
            loop i
          else loop (i-1))
  in loop (h-1)

let valid offsetX offsetY board {shape=current; currentX; currentY} =
  let offsetX = currentX + offsetX in
  let offsetY = currentY + offsetY in
  let w, h = width board, height board in
  let rec y_x_loop y x =
    if x >= 4 then y_x_loop (y+1) 0 else
    if y >= 4 then (false, true) else
    if get y x current <> 0 then
      let xo, yo = x + offsetX, y + offsetY in
      if (yo < 0
          || xo < 0
          || yo >= h
          || xo >= w
          || get yo xo board <> 0)
      then (offsetY = 1, false)
      else y_x_loop y (x+1)
    else y_x_loop y (x+1) in
  y_x_loop 0 0

let tick board current =
  let new_current = {current with currentY=current.currentY+1} in
  let lose, vld = valid 0 0 board new_current in
  if vld then (board, new_current, false)
  else 
    (freeze board current;
     clear_lines board;
     if lose then (board, new_shape (width board / 2) (), true)
     else (board, new_shape (width board / 2) (), false))

let tick board_ref current_ref () =
  let b, c, l =  tick !board_ref !current_ref in
  if l then (update (fun _ -> 0) !board_ref; current_ref := new_shape (width !board_ref / 2) ())
  else board_ref := b; current_ref := c
