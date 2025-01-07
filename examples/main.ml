open Fs

(** Example usage of the File module *)
let test_files () =
  (* Write a string to a file *)
  let () =
    match File.write_string "example.txt" ~content:"Hello, World!" with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Read the file contents *)
  let () =
    match File.read_string "example.txt" with
    | Ok content -> Printf.printf "File content: %s\n" content
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Check if a file exists *)
  let () =
    match File.exists "example.txt" with
    | Ok true -> Printf.printf "File exists\n"
    | Ok false -> Printf.printf "File does not exist\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Clean up *)
  let () =
    match File.delete "example.txt" with
    | Ok () -> Printf.printf "File deleted successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Write bytes to a file *)
  let () =
    match File.write_bytes "example.txt" ~content:(Bytes.of_string "Hello, World!") with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Read the file contents as bytes *)
  let () =
    match File.read_bytes "example.txt" with
    | Ok bytes -> Printf.printf "File content: %s\n" (Bytes.to_string bytes)
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Write lines to a file *)
  let () =
    match File.write_string "example.txt" ~content:"Hello\nWorld\n!" with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Read the file contents as lines *)
  let () =
    match File.read_lines "example.txt" with
    | Ok lines -> lines |> List.iter (fun line -> Printf.printf "Line: %s\n" line)
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* Clean up *)
  match File.delete "example.txt" with
  | Ok () -> Printf.printf "File deleted successfully\n"
  | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
;;

(** Example usage of the Dir module *)

let test_dirs () =
  (* Create a directory with sub-directories *)
  let () =
    match Dir.create "example/nested" ~recursive:true () with
    | Ok () -> Printf.printf "Directories created successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (Dir.Error.to_string e)
  in

  (* Check if directory exists *)
  let () =
    match Dir.exists "example" with
    | Ok true -> Printf.printf "Directory exists\n"
    | Ok false -> Printf.printf "Directory does not exist\n"
    | Error e -> Printf.printf "Error: %s\n" (Dir.Error.to_string e)
  in

  (* Create a file in the directory *)
  let () =
    match File.write "example/file1.txt" ~content:(String "File 1") ~append:false () with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
  in

  (* List directory contents *)
  let () =
    match Dir.list "example" () with
    | Ok entries ->
      entries |> List.iter (fun entry -> print_endline (Dir.Entry.get_name entry));
      (* or *)
      entries
      |> List.iter (function
        | Dir.File file -> Printf.printf "File: %s\n" (File.get_name file)
        | Dir.Directory dir -> Printf.printf "Directory: %s\n" (Dir.get_name dir))
    | Error e -> Printf.printf "Error: %s\n" (Dir.Error.to_string e)
  in

  (* Clean up *)
  match Dir.delete "example" ~recursive:true () with
  | Ok () -> Printf.printf "Directory deleted successfully\n"
  | Error e -> Printf.printf "Error: %s\n" (Dir.Error.to_string e)
;;

let () =
  test_files ();
  test_dirs ()
;;
