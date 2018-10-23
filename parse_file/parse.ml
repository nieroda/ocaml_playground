

 module SSU_Class = struct
  type t = {
    time: Time_ns.t;
    teacher: string;
    title: string;
    hours_of_work_week: string;
    comment: string;
  }
  [@@deriving fields]
end ;;


let create_ssu_class ~teacher ~title ~hours_of_work_week ~comment : SSU_Class.t =
  { teacher; title; hours_of_work_week; comment; time = Time_ns.now() }
  ;;

let get_str_option str_option =
  match str_option with
  | None -> ""
  | Some x -> x
  ;;


let print_teacher_name lst =
  List.map lst ~f:(fun line ->
    printf "%s\t" (SSU_Class.teacher line) ;
    printf "%s\n" (SSU_Class.comment line) ;
  )
  ;;


let info_from_string line =
  let (line, comment) =
    match String.rsplit2 line ~on:'#' with
    | None -> (line, None)
    | Some (o, c) -> (o, Some c)
    in
    let matches = Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)/([0-9]+)/([0-9]+)") line in
      let teacher = Re.get matches 1 in
      let title = Re.get matches 2 in
      let hours_of_work_week = Re.get matches 3 in
      create_ssu_class ~teacher:teacher ~title:title ~hours_of_work_week:hours_of_work_week ~comment:(get_str_option comment)
;;


let read_file file_name =
  Sys.chdir "../../";
  In_channel.create file_name
  |> In_channel.input_lines ~fix_win_eol:true
  |> List.map ~f:info_from_string
  |> print_teacher_name

;;
read_file "example.txt"
;;
