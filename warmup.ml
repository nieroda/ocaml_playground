let normal_dates = [
  ("Jan", 31);
  ("Feb", 28);
  ("Mar", 31);
  ("Apr", 30);
  ("May", 31);
  ("Jun", 30);
  ("Jul", 31);
  ("Aug", 31);
  ("Sep", 30);
  ("Oct", 31);
  ("Nov", 30);
  ("Dec", 31);
]

let leap_dates = [
  ("Jan", 31);
  ("Feb", 29);
  ("Mar", 31);
  ("Apr", 30);
  ("May", 31);
  ("Jun", 30);
  ("Jul", 31);
  ("Aug", 31);
  ("Sep", 30);
  ("Oct", 31);
  ("Nov", 30);
  ("Dec", 31);
]

let rec match_month m leap_dates =
  match leap_dates with
  | [] -> None
  | (hd::tl) ->
    let (key, value) = hd in
    if key = m then Some (value)
    else match_month m tl;;

let is_leap y =
  if (y mod 4 = 0 && y mod 100 != 0) || y mod 400 = 0 then leap_dates else normal_dates;;

let valid_date y m d =
  let is_valid_year = y >= 1 in
  let is_leap = is_leap y in
  match (match_month m is_leap) with
  | None -> false
  | Some x -> if 0 <= d && d <= x
    then is_valid_year
    else false


let rec syr_help n cnt =
  if n = 1 then cnt else
  match (n mod 2 = 0) with
  | true -> syr_help (n / 2) (cnt + 1)
  | false -> syr_help (3 * n + 1) (cnt + 1)


let syr n =
  syr_help n 0


let rec get_prev_n lst len n =
  (* print_endline ("Called..len is" ^ (string_of_int len) ^ "and n is " ^ (string_of_int n)); *)
  match lst with
  | [] -> 0
  | (hd::tl) -> if len <= n
    then hd + get_prev_n tl (len - 1) n
    else get_prev_n tl (len - 1) n
;;

let rec nacci_helper n k lst =
  (* print_endline "Called"; *)
  (* print_list lst; *)
  if k = 1 then [1]
  else if k = 2 then [1; 1]
  else nacci_helper n (k - 1) lst @[get_prev_n lst (List.length lst) n]



let nacci n k =

  failwith "not working :("

open OUnit2
open Warmup

(* ocamlbuild -pkgs oUnit warmup_test.byte *)

let vd_tests = "Valid Date" >::: [
  "Valid Date"         >:: (fun _ -> assert_equal true  (valid_date 2020 "Feb" 29));
  "Invalid Day"        >:: (fun _ -> assert_equal false (valid_date 2010 "Jun" 50));
  "Test_L Year #1"     >:: (fun _ -> assert_equal true  (valid_date 2400 "Feb" 29));
  "Test_L Year #2"     >:: (fun _ -> assert_equal false (valid_date 2011 "Feb" 29));
]

let syr_tests = "Syr Tests" >::: [
  "Test Resp"  >:: (fun _ -> assert_equal 1 (syr 2));
  "Test Syr"   >:: (fun _ -> assert_equal 6 (syr 10));
]

let () =
  run_test_tt_main vd_tests;
  run_test_tt_main syr_tests;
