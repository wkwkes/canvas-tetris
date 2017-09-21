open Util

let colors = [|
  "cyan"; "orange"; "blue"; "yellow"; "red"; "green"; "purple"
|]


let draw_block ctx block_w block_h x y =
  fillRect ctx (block_w * x) (block_h * y) (block_w - 1) (block_h - 1);
  strokeRect ctx (block_w * x) (block_h * y) (block_w - 1) (block_h - 1)

let render board current ws hs ctx () = 
  let {shape=current; currentX; currentY} = !current in
  let cols, rows = Board.width !board, Board.height !board in
  clearRect ctx 0 0 ws hs;
  strokeStyle ctx "black";
  for x = 0 to cols - 1 do
    for y = 0 to rows - 1 do 
      let clr = Board.get y x !board in
      if clr <> 0 then
        (fillStyle ctx @@ colors.((clr - 1) mod 7);
         draw_block ctx (ws / cols) (hs / rows) x y)
    done
  done;
  fillStyle ctx "red";
  strokeStyle ctx "black";
  for y = 0 to 3 do
    for x = 0 to 3 do
      let clr = Board.get y x current in
      if clr <> 0 then
        (fillStyle ctx @@ colors.((clr - 1) mod 7);
         draw_block ctx (ws / cols) (hs / rows) (currentX + x) (currentY + y))
    done
  done
