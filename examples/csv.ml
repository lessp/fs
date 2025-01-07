open Fs

let ( let* ) = Result.bind

let create_csv () =
  let csv_content = "Name,Age,City\nAlice,25,Paris\nBob,30,London\n" in
  File.write_string "data.csv" ~content:csv_content
;;

let print_csv_line line =
  match String.split_on_char ',' line with
  | [ name; age; city ] -> Printf.printf "Name: %s, Age: %s, City: %s\n" name age city
  | _ -> Printf.printf "Invalid line: %s\n" line
;;

let process_csv () =
  let* () = create_csv () in
  let* lines = File.read_lines "data.csv" in

  let () =
    match lines with
    | [] -> Printf.printf "No data found\n"
    | headers :: entries ->
      Printf.printf "Headers: %s\n" headers;
      entries |> List.iter print_csv_line
  in

  File.delete "data.csv"
;;

let () =
  match process_csv () with
  | Ok () -> print_endline "CSV processed successfully"
  | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
;;
