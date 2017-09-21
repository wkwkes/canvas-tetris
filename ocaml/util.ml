
module Board : sig
  type 'a t
  exception Invalid_copy of int * int * int * int
  val make : int -> int -> 'a -> 'a t
  val width : 'a t -> int
  val height : 'a t -> int
  val set : int -> int -> 'a -> 'a t -> unit
  val set_row : int -> 'a t -> 'a array -> unit
  val get_row : int -> 'a t -> 'a array
  val get : int -> int -> 'a t -> 'a
  val update_row : int -> ('a -> 'a) -> 'a t -> unit
  val update : ('a -> 'a) -> 'a t -> unit
  val of_matrix : 'a array array -> 'a t
  val copy : 'a t -> 'a t -> 'a -> int -> int -> unit
  val print : ('a -> unit) -> 'a t ->  unit
  val rotate : 'a t -> unit
  val rotate_rev : 'a t -> unit
  val dup : 'a t -> 'a t
  val for_all_row : ('a -> bool) -> int -> 'a t -> bool
  val slice : int * int -> int * int -> 'a t -> 'a t
end = struct
  type 'a t = 'a array array
  exception Invalid_copy of int * int * int * int
  let make i j init = Array.make_matrix i j init
  let height l = Array.length l
  let width l = Array.length l.(0)
  let set i j a b = b.(i).(j) <- a
  let set_row i b row = b.(i) <- row
  let get_row i b = b.(i)
  let get i j b = b.(i).(j)
  let update_row i f b =
    let b = b.(i) in
    for i = 0 to Array.length b - 1 do
      b.(i) <- f b.(i)
    done
  let update f b =
    for i = 0 to Array.length b - 1 do
      update_row i f b
    done
  let of_matrix m = m
  let copy b1 b2 def y x =
    let h1, w1 = height b1, width b1 in
    for i = 0 to h1 - 1 do
      for j = 0 to w1 - 1 do
        if b1.(i).(j) <> def then b2.(i+y).(j+x) <- b1.(i).(j)
      done
    done
  let print p b =
    for i = 0 to height b - 1 do
      for j = 0 to width b - 1 do
        p b.(i).(j);
        if j <> width b - 1 then print_string " | "
      done;
      print_newline ()
    done
  let rotate b =
    let h, w = height b, width b in
    let cashe = make h w None in
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        cashe.(j).(w-i-1) <- Some b.(j).(w-i-1);
        match cashe.(i).(j) with
        | None -> b.(j).(w-i-1) <- b.(i).(j)
        | Some x -> b.(j).(w-i-1) <- x
      done
    done
  let rotate_rev b =
    let h, w = height b, width b in
    let cashe = make h w None in
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        cashe.(i).(j) <- Some b.(i).(j);
        match cashe.(j).(w-i-1) with
        | None -> b.(i).(j) <- b.(j).(w-i-1)
        | Some x -> b.(i).(j) <- x
      done
    done  
  let dup b = Array.init (height b) (fun i -> Array.copy b.(i))
  let for_all_row f i b =
    try
      for j = 0 to width b - 1 do
        if not (f b.(i).(j)) then failwith ""
      done;
      true
    with
    | _ -> false
  let slice (y1, x1) (y2, x2) b =
    let new_b = make (y2 - y1) (x2 - x1) b.(0).(0) in
    for i = 0 to y2 - y1 - 1 do
      for j = 0 to x2 - x1 - 1 do
        new_b.(i).(j) <- b.(y1 + i).(x1 + j)
      done
    done;
    new_b
end

let print = Board.print print_int
let hoge = Board.make 5 5 0
let foo = ref 0
let () = Board.update (fun _ -> foo := !foo + 1; !foo) hoge


type shape = {shape : int Board.t; currentX : int; currentY : int}

(***** js bindings *****)

type id (** Abstract type for id object *)
external get_by_id : string -> id =
  "getElementById" [@@bs.scope "document"] [@@bs.val]

external play : id -> unit =
  "" [@@bs.send]

external clearInterval : int -> unit = "" [@@bs.val]
external setInterval : ('a Board.t -> bool) -> int -> int = "" [@@bs.val]

type canvas (** Abstract type for canvas *)
type canvas2d

external getElementsByTagName : string -> id = "" [@@bs.scope "document"] [@@bs.val]
external get_canvas : id -> int -> canvas = "" [@@bs.get_index]
external getContext : canvas -> string -> canvas2d = "" [@@bs.send]
external fillRect : canvas2d -> int -> int -> int -> int -> unit = "" [@@bs.send]
external strokeRect : canvas2d -> int -> int -> int -> int -> unit = "" [@@bs.send]
external clearRect : canvas2d -> int -> int -> int -> int -> unit = "" [@@bs.send]
external strokeStyle : canvas2d -> string -> unit = "" [@@bs.set]
external fillStyle : canvas2d -> string -> unit = "" [@@bs.set]

type key_event
type dom
external dom : dom = "document" [@@bs.val]
external onkeydown : dom -> (key_event -> unit) -> unit = "" [@@bs.scope "body"] [@@bs.set]
external keyCode : key_event -> int = "" [@@bs.get]
