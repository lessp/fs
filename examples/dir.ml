open Fs

let ( let* ) = Result.bind

let dir1 () =
  let* entries = Dir.list "." () in
  entries |> List.iter (fun entry -> Printf.printf "%s\n" (Dir.Entry.get_name entry));
  Ok ()
;;

let dir2 () =
  let* entries = Dir.list "." ~dotfiles:true () in
  entries
  |> List.iter (function
    | Dir.File file -> Printf.printf "File: %s\n" (File.get_name file)
    | Dir.Directory dir -> Printf.printf "Directory: %s\n" (Dir.get_name dir));
  Ok ()
;;

let () =
  let test () =
    let* () = dir1 () in
    print_endline "----------------";
    let* () = dir2 () in
    Ok ()
  in

  match test () with
  | Ok () -> Printf.printf "Test successful\n"
  | Error e -> Printf.printf "Error: %s\n" (Dir.Error.to_string e)
;;
