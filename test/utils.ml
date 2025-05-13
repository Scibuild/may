open! Core

let remove_indentation s =
  let lines = s |> String.strip ~drop:(Char.equal '\n') |> String.split_lines in
  let shortest_indent =
    lines
    |> List.filter_map ~f:(fun line ->
      match String.is_empty line with
      | true -> None
      | false -> String.take_while line ~f:(Char.equal ' ') |> String.length |> Some)
    |> List.min_elt ~compare:Int.compare
  in
  match shortest_indent with
  | None -> ""
  | Some shortest_indent ->
    lines
    |> List.map ~f:(fun line ->
      match String.is_empty line with
      | true -> line
      | false -> String.drop_prefix line shortest_indent)
    |> String.concat ~sep:"\n"
;;

let%expect_test "test removing indentation" =
  print_endline (remove_indentation "    hello");
  [%expect {| hello |}];
  print_endline (remove_indentation "    hello\n    more_hello");
  [%expect
    {|
    hello
    more_hello
    |}]
;;
