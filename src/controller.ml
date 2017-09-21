open Util
open Tetris

let controller_set board_ref current_ref ws hs ctx =
  let rend = Render.render board_ref current_ref ws hs ctx in
  onkeydown dom  
    (fun e ->
       match Util.keyCode e with
       | 37 (* left *) -> if snd @@ valid (-1) 0 !board_ref !current_ref then
             current_ref := {!current_ref with currentX = !current_ref.currentX - 1}; rend ()
       | 39 (* right *) -> if snd @@ valid 1 0 !board_ref !current_ref then
             current_ref := {!current_ref with currentX = !current_ref.currentX + 1}; rend ()
       | 40 (* down *) -> if snd @@ valid 0 1 !board_ref !current_ref then
             current_ref := {!current_ref with currentY = !current_ref.currentY + 1}; rend ()
       | 38 (* rotate *) -> 
           Board.rotate !current_ref.shape; (* TODO *)
           if not @@ snd @@ valid 0 0 !board_ref !current_ref then
             Board.rotate_rev !current_ref.shape; rend ()
       | _ -> ())
