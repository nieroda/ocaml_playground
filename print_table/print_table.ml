(* w/o soln *)
open Core ;;

(*
https://ocaml.janestreet.com/ocaml-core/109.08.00/doc/core/String.html
https://ocaml.janestreet.com/ocaml-core/v0.10/doc/base/Base/index.html
*)

let max_width header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun accum row ->
      List.map2_exn ~f:Int.max accum (lengths row));
  ;;

let render_sep width =
  let pieces = List.map width ~f:(fun word -> String.make (word + 2) '-') in
      "|" ^ String.concat ~sep:"|" pieces ^ "|"
  ;;

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '
  ;;

let render_row row width =
  let padded = List.map2_exn ~f:pad row width in
  "|" ^ String.concat ~sep:"|" padded ^ "|"
  ;;

let render_table header rows =
  let widths = max_width header rows in
    String.concat ~sep:"\n" (
      render_row header widths
      :: render_sep widths
      :: List.map rows ~f:(fun row -> render_row row widths)
    )
  ;;

Stdio.print_endline
  (render_table
    ["Class"; "Teacher"; "Num Students"]
    [["CS 351"; "Kooshesh"; "50"] ;
     ["CS 315"; "Kooshesh"; "40"] ;
     ["CS 460"; "Gondree"; "30" ] ;
    ]
  );;
