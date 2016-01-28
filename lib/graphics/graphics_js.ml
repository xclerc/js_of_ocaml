(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

exception Graphic_failure of string

(* Initializations *)

let _ =
  Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let open_graph: string -> unit = Js.Unsafe.pure_js_expr "caml_gr_open_graph"
let close_graph: unit -> unit = Js.Unsafe.pure_js_expr "caml_gr_close_graph"

let set_window_title : string -> unit = Js.Unsafe.pure_js_expr "caml_gr_set_window_title"
let resize_window : int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_resize_window"
let clear_graph : unit -> unit = Js.Unsafe.pure_js_expr "caml_gr_clear_graph"
let size_x : unit -> int = Js.Unsafe.pure_js_expr "caml_gr_size_x"
let size_y : unit -> int = Js.Unsafe.pure_js_expr "caml_gr_size_y"

(* Double-buffering *)

let display_mode : bool -> unit = Js.Unsafe.pure_js_expr "caml_gr_display_mode"
let remember_mode : bool -> unit = Js.Unsafe.pure_js_expr "caml_gr_remember_mode"
let synchronize : unit -> unit = Js.Unsafe.pure_js_expr "caml_gr_synchronize"

let auto_synchronize = function
  | true -> display_mode true; remember_mode true; synchronize ()
  | false -> display_mode false; remember_mode true
;;


(* Colors *)

type color = int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

let set_color : color -> unit = Js.Unsafe.pure_js_expr "caml_gr_set_color"

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF

let background = white
and foreground = black

(* Drawing *)

let plot : int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_plot"
let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y;
  done
;;
let point_color : int -> int -> color = Js.Unsafe.pure_js_expr "caml_gr_point_color"
let moveto : int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_moveto"
let current_x : unit -> int = Js.Unsafe.pure_js_expr "caml_gr_current_x"
let current_y : unit -> int = Js.Unsafe.pure_js_expr "caml_gr_current_y"
let current_point () = current_x (), current_y ()
let lineto : int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_lineto"
let rlineto x y = lineto (current_x () + x) (current_y () + y)
let rmoveto x y = moveto (current_x () + x) (current_y () + y)

let raw_draw_rect : int -> int -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_draw_rect"
let draw_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "draw_rect")
  else raw_draw_rect x y w h
;;

let draw_poly, draw_poly_line =
  let dodraw close_flag points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      if close_flag then lineto (fst points.(0)) (snd points.(0));
      moveto savex savey;
    end;
  in dodraw true, dodraw false
;;
let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;
;;

let raw_draw_arc : int -> int -> int -> int -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_draw_arc"
let draw_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "draw_arc/ellipse/circle")
  else raw_draw_arc x y rx ry a1 a2
;;

let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360
let draw_circle x y r = draw_arc x y r r 0 360

let raw_set_line_width : int -> unit = Js.Unsafe.pure_js_expr "caml_gr_set_line_width"
let set_line_width w =
  if w < 0 then raise (Invalid_argument "set_line_width")
  else raw_set_line_width w
;;

let raw_fill_rect : int -> int -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_fill_rect"
let fill_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "fill_rect")
  else raw_fill_rect x y w h
;;

let fill_poly : (int * int) array -> unit = Js.Unsafe.pure_js_expr "caml_gr_fill_poly"
let raw_fill_arc : int -> int -> int -> int -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_fill_arc"
let fill_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "fill_arc/ellipse/circle")
  else raw_fill_arc x y rx ry a1 a2
;;

let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360

(* Text *)

let draw_char : char -> unit = Js.Unsafe.pure_js_expr "caml_gr_draw_char"
let draw_string : string -> unit = Js.Unsafe.pure_js_expr "caml_gr_draw_string"
let set_font : string -> unit = Js.Unsafe.pure_js_expr "caml_gr_set_font"
let set_text_size : int -> unit = Js.Unsafe.pure_js_expr "caml_gr_set_text_size"
let text_size : string -> int * int = Js.Unsafe.pure_js_expr "caml_gr_text_size"

(* Images *)

type image

let transp = -1

let make_image : color array array -> image = Js.Unsafe.pure_js_expr "caml_gr_make_image"
let dump_image : image -> color array array = Js.Unsafe.pure_js_expr "caml_gr_dump_image"
let draw_image : image -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_draw_image"
let create_image : int -> int -> image = Js.Unsafe.pure_js_expr "caml_gr_create_image"
let blit_image : image -> int -> int -> unit = Js.Unsafe.pure_js_expr "caml_gr_blit_image"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image

(* Events *)

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char }

type event =
    Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

let wait_next_event : event list -> status = Js.Unsafe.pure_js_expr "caml_gr_wait_event"

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y)

let button_down () =
  let e = wait_next_event [Poll] in e.button

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed

let loop_at_exit events handler =
  let events = List.filter (fun e -> e <> Poll) events in
  at_exit (fun _ ->
    try
      while true do
        let e = wait_next_event events in
        handler e
      done
    with Exit -> close_graph ()
       | e -> close_graph (); raise e
  )

(* Splines *)
let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
and sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
and middle (x1, y1) (x2, y2) = ((x1 +. x2) /. 2.0,  (y1 +. y2) /. 2.0)
and area (x1, y1) (x2, y2) = abs_float (x1 *. y2 -. x2 *. y1)
and norm (x1, y1) = sqrt (x1 *. x1 +. y1 *. y1);;

let test a b c d =
 let v = sub d a in
 let s = norm v in
 area v (sub a b) <= s && area v (sub a c) <= s;;

let spline a b c d =
  let rec spl accu a b c d =
   if test a b c d then d :: accu else
   let a' = middle a b
   and o = middle b c in
   let b' = middle a' o
   and d' = middle c d in
   let c' = middle o d' in
   let i = middle b' c' in
   spl  (spl accu a a' b' i) i c' d' d in
  spl [a] a b c d;;

let curveto b c (x, y as d) =
 let float_point (x, y) = (float_of_int x, float_of_int y) in
 let round f = int_of_float (f +. 0.5) in
 let int_point (x, y) = (round x, round y) in
 let points =
   spline
    (float_point (current_point ()))
    (float_point b) (float_point c) (float_point d) in
 draw_poly_line
  (Array.of_list (List.map int_point points));
 moveto x y;;

(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type context
let _ = Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let (>>=) = Lwt.bind

let get_context () =
  Js.Unsafe.(fun_call (variable "caml_gr_state_get") [| |])

let set_context ctx =
  Js.Unsafe.(fun_call (variable "caml_gr_state_set") [| inject ctx |])

let create_context canvas w h =
  Js.Unsafe.(fun_call (variable "caml_gr_state_create")
               [| inject canvas; inject w; inject h|])

let document_of_context ctx =
  Js.Unsafe.(fun_call (variable "caml_gr_doc_of_state") [| inject ctx |])

let open_canvas x =
  let ctx = create_context x x##width x##height in
  set_context ctx

let compute_real_pos elt =
  let rec loop elt left top =
    let top = elt##offsetTop - elt##scrollTop + top
    and left = elt##offsetLeft - elt##scrollLeft + left in
    match Js.Opt.to_option elt##offsetParent with
    | None -> top,left
    | Some p -> loop p left top
  in loop elt 0 0

let mouse_pos () =
  let ctx = get_context() in
  let elt = ctx##canvas in
  Lwt_js_events.mousemove elt >>= fun ev ->
  let top,left = compute_real_pos elt in
  Lwt.return ((Js.Optdef.get (ev##pageX) (fun _ -> 0)) - left,
              elt##height - ((Js.Optdef.get (ev##pageY) (fun _ -> 0)) - top))

let button_down () =
  let ctx = get_context() in
  let elt = ctx##canvas in
  Lwt_js_events.mousedown elt >>= fun _ev ->
  Lwt.return true

let read_key () =
  (* let ctx = get_context() in *)
  (* let elt = ctx##canvas in *)
  let doc = document_of_context (get_context ()) in
  Lwt_js_events.keypress doc >>= fun ev ->
  Lwt.return (Char.chr ev##keyCode)

let loop elist f : unit =
  let ctx = get_context() in
  let elt = ctx##canvas in
  let doc = document_of_context (get_context ()) in
  let button = ref false in
  let null = char_of_int 0 in
  let mouse_x, mouse_y = ref 0, ref 0 in

  let get_pos_mouse () = !mouse_x, !mouse_y in

  if List.mem Button_down elist then
    elt##onmousedown <- Dom_html.handler (fun _ev ->
        let mouse_x, mouse_y = get_pos_mouse () in
        button := true;
        let s = { mouse_x ; mouse_y ; button=true ;
                  keypressed=false ; key=null } in
        f s;
        Js._true);

  if List.mem Button_up elist then
    elt##onmouseup <- Dom_html.handler (fun _ev ->
        let mouse_x, mouse_y = get_pos_mouse () in
        button := false;
        let s = { mouse_x ; mouse_y ; button=false ;
                  keypressed=false ; key=null } in
        f s;
        Js._true);


  elt##onmousemove <- Dom_html.handler (fun ev ->
      let cy,cx = compute_real_pos elt in
      mouse_x := (Js.Optdef.get (ev##pageX) (fun _ -> 0)) - cx;
      mouse_y := elt##height - (Js.Optdef.get (ev##pageY) (fun _ -> 0) - cy);
      if List.mem Mouse_motion elist then
        (let mouse_x, mouse_y = get_pos_mouse () in
         let s = { mouse_x ; mouse_y ; button=(!button) ;
                   keypressed=false ; key=null } in
         f s);
      Js._true);

  (* EventListener sur le doc car pas de moyen simple de le faire
     sur un canvasElement *)
  if List.mem Key_pressed elist then
    doc##onkeypress <- Dom_html.handler (fun ev ->
        (* Uncaught Invalid_argument char_of_int with key € for example *)
        let key =
          try char_of_int (Js.Optdef.get (ev##charCode) (fun _ -> 0))
          with Invalid_argument _ -> null in
        let mouse_x, mouse_y = get_pos_mouse () in
        let s = { mouse_x ; mouse_y ; button=(!button) ;
                  keypressed=true ; key } in
        f s;
        Js._true)

let loop_at_exit events handler : unit =
  at_exit (fun _ -> loop events handler)
