open Fs

let ( let* ) = Result.bind

type diff_type =
  | Added of string
  | Removed of string
  | Changed of
      { old_text : string
      ; new_text : string
      }
  | Unchanged of string

type diff_result =
  { line_number : int
  ; diff : diff_type
  }

let colorize text color =
  match color with
  | `Red -> Printf.sprintf "\027[31m%s\027[0m" text
  | `Green -> Printf.sprintf "\027[32m%s\027[0m" text
  | `Yellow -> Printf.sprintf "\027[33m%s\027[0m" text
  | `Grey -> Printf.sprintf "\027[90m%s\027[0m" text
;;

let create_test_files () =
  let original =
    [ "This is a test file"
    ; "It has multiple lines"
    ; "Some lines will change"
    ; "This line stays the same"
    ; "This line will be removed"
    ; "Another unchanged line"
    ]
  in

  let modified =
    [ "This is a test file"
    ; "It has several lines now"
    ; "Some lines have changed"
    ; "This line stays the same"
    ; "A new line was added here"
    ; "Another unchanged line"
    ; "And one more new line"
    ]
  in

  let* () = File.write_lines "original.txt" original in
  let* () = File.write_lines "modified.txt" modified in

  Ok ()
;;

let compare_files a b =
  let* original = File.read_lines a in
  let* modified = File.read_lines b in

  let make_diff line_num diff_type = { line_number = line_num; diff = diff_type } in

  let rec compare original modified line_num results =
    match original, modified with
    | [], [] -> Ok (List.rev results)
    (* Original file is done, but modified file has additional lines *)
    | [], added_line :: remaining_modified ->
      let diff = make_diff line_num (Added added_line) in
      compare [] remaining_modified (line_num + 1) (diff :: results)
    (* Modified file is done, but original file has remaining lines *)
    | removed_line :: remaining_original, [] ->
      let diff = make_diff line_num (Removed removed_line) in
      compare remaining_original [] (line_num + 1) (diff :: results)
    (* Both files have remaining lines *)
    | original_line :: remaining_original, modified_line :: remaining_modified ->
      let diff =
        if String.equal original_line modified_line then
          make_diff line_num (Unchanged original_line)
        else
          make_diff
            line_num
            (Changed { old_text = original_line; new_text = modified_line })
      in
      compare remaining_original remaining_modified (line_num + 1) (diff :: results)
  in

  compare original modified 1 []
;;

let print_diff results =
  List.iter
    (fun { line_number; diff } ->
      match diff with
      | Added text ->
        Printf.printf
          "%3d: %s %s\n"
          line_number
          (colorize "+" `Green)
          (colorize text `Green)
      | Removed text ->
        Printf.printf "%3d: %s %s\n" line_number (colorize "-" `Red) (colorize text `Red)
      | Changed { old_text; new_text } ->
        Printf.printf
          "%3d: %s %s\n     %s %s\n"
          line_number
          (colorize "-" `Red)
          (colorize old_text `Red)
          (colorize "+" `Green)
          (colorize new_text `Green)
      | Unchanged text ->
        Printf.printf
          "%3d: %s %s\n"
          line_number
          (colorize " " `Grey)
          (colorize text `Grey))
    results
;;

let demo_diff () =
  let* () = create_test_files () in

  Printf.printf "\nComparing files:\n";
  Printf.printf "=================\n\n";

  let* diffs = compare_files "original.txt" "modified.txt" in
  print_diff diffs;

  Printf.printf "\nCleanup:\n";
  Printf.printf "========\n";

  let* () = File.delete "original.txt" in
  let* () = File.delete "modified.txt" in

  Printf.printf "Test files cleaned up\n";
  Ok ()
;;

let () =
  match demo_diff () with
  | Ok () -> print_endline "\nDiff completed successfully"
  | Error e -> Printf.printf "\nError: %s\n" (File.Error.to_string e)
;;
