open Tetris
open Util
open Util.Board


let new_game row col =
  let board = ref @@ make row col 0 in
  let current = ref @@ new_shape (col / 2) () in
  let canvas = get_canvas (getElementsByTagName "canvas") 0 in
  let ctx = getContext canvas "2d" in
  let w_size, h_size = 300, 600 in
  let tick = tick board current in
  let render = Render.render board current w_size h_size ctx in
  let () = Controller.controller_set board current w_size h_size ctx in
  let play () = render(); tick () in
  ignore @@ Js.Global.setInterval play 500

let main = 
  new_game 20 10
