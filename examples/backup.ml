open Fs

let ( let* ) = Result.bind

let backup_file path =
  let backup_path = path ^ ".bak" in

  let* content = File.read_string path in
  let* () = File.write_string backup_path ~content in

  Printf.printf "Backup created: %s\n" backup_path;

  (* Verify backup *)
  let* backup_content = File.read_string backup_path in
  if String.equal backup_content content then
    Ok "Backup verified successfully"
  else
    Error (`FileWriteError backup_path)
;;

let () =
  (* Create test file *)
  File.write_string "test.txt" ~content:"Important data" |> Result.get_ok;

  match backup_file "test.txt" with
  | Ok msg ->
    print_endline msg;
    (* Clean up *)
    File.delete "test.txt" |> Result.get_ok;
    File.delete "test.txt.bak" |> Result.get_ok
  | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
;;
