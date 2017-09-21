open Util
open Util.Board


(* constants *)
let cols = 10
let rows = 20
let w_size = 300
let h_size = 600
let canvas = get_canvas (getElementsByTagName "canvas") 0
let ctx = getContext canvas "2d"

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

let new_shape () =
  let id = Array.length shapes |> Random.int in
  let current = shapes.(id) |> dup in
  update (fun x -> x * (id+1)) current;
  { shape=current; x=(cols/2); y=0 }

let freeze board {shape; x; y} =
  copy shape board y x

let clear_lines board =
  let line_is_full i = for_all_row (( <> ) 0) i board in
  let rec loop i =
    if i < 0 then () 
    else (if line_is_full i 
          then
            let sound = get_by_id "clearsound" in play sound;
            for j = i downto 1 do
              set_row j board (get_row (j-1) board);
            done;
            set_row 0 board (Array.make cols 0);
            loop i
          else loop (i-1))
  in loop (rows-1)

let valid offsetX offsetY board {shape; x; y} =
  let offsetX = x + offsetX in
  let offsetY = y + offsetY in
  let rec y_x_loop y x =
    if x >= 4 then y_x_loop (y+1) 0 else
    if y >= 4 then (false, true) else
    if get y x shape <> 0 then
      let xo, yo = x + offsetX, y + offsetY in
      if (yo < 0 || xo < 0 || yo >= rows || xo >= cols
          || get yo xo board <> 0)
      then (offsetY = 1, false)
      else y_x_loop y (x+1)
    else y_x_loop y (x+1) in
  y_x_loop 0 0

let tick board current =
  let new_cur = {current with y=current.y+1} in
  let lose, vld = valid 0 0 board new_cur in
  if vld then (board, new_cur, false)
  else (freeze board current;
        clear_lines board;
        if lose then (board, new_shape (), true)
        else (board, new_shape (), false))

let tick board_ref current_ref () =
  let b, c, l =  tick !board_ref !current_ref in
  if l then (update (fun _ -> 0) !board_ref; current_ref := new_shape ())
  else board_ref := b; current_ref := c

let colors = [|
  "cyan"; "orange"; "blue"; "yellow"; "red"; "green"; "purple"
|]

let draw_block x y =
  let w, h = w_size / cols, h_size / rows in
  fillRect ctx (w * x) (h * y) (w - 1) (h - 1);
  strokeRect ctx (w * x) (h * y) (w - 1) (h - 1)

let render board current () = 
  let {shape; x; y} = !current in
  clearRect ctx 0 0 w_size h_size;
  strokeStyle ctx "black";
  for i = 0 to cols - 1 do
    for j = 0 to rows - 1 do 
      let clr = get j i !board in
      if clr <> 0 then
        (fillStyle ctx @@ colors.((clr - 1) mod 7);
         draw_block i j)
    done
  done;
  fillStyle ctx "red";
  strokeStyle ctx "black";
  for i = 0 to 3 do
    for j = 0 to 3 do
      let clr = get i j shape in
      if clr <> 0 then
        (fillStyle ctx @@ colors.((clr - 1) mod 7);
         draw_block (x + j) (y + i))
    done
  done

let controller_set b_ref c_ref =
  let rend = render b_ref c_ref in
  onkeydown dom
    (fun e ->
       match Util.keyCode e with
       | 37 (* left *) when snd @@ valid (-1) 0 !b_ref !c_ref ->
           c_ref := {!c_ref with x = !c_ref.x - 1}; rend ()
       | 39 (* right *) when snd @@ valid 1 0 !b_ref !c_ref ->
           c_ref := {!c_ref with x = !c_ref.x + 1}; rend ()
       | 40 (* down *) when snd @@ valid 0 1 !b_ref !c_ref ->
           c_ref := {!c_ref with y = !c_ref.y + 1}; rend ()
       | 38 (* rotate *) -> 
           Board.rotate !c_ref.shape;
           if not @@ snd @@ valid 0 0 !b_ref !c_ref then
             Board.rotate_rev !c_ref.shape; rend ()
       | _ -> ())

let new_game =
  let board = ref @@ make rows cols 0 in
  let current = ref @@ new_shape () in
  let tick = tick board current in
  let render = render board current in
  let () = controller_set board current in
  let play () = render(); tick () in
  ignore @@ Js.Global.setInterval play 500
