open Fs

let () =
  (* Write *)
  match File.write "my-test.txt" ~contents:(`String "Hello, world!") with
  | Ok () -> Printf.printf "File written successfully\n"
  | Error e -> Printf.printf "Error writing file: %s" (Error.to_string e)

let () =
  (* Read - Fail *)
  match File.read "my-test-does-not-exist.txt" ~format:`String with
  | Ok f -> Printf.printf "File read successfully: %s\n" (File.get_name f)
  | Error e -> Printf.printf "%s\n" (Error.to_string e)

let () =
  (* Read - Success *)
  match File.read "my-test.txt" ~format:`String with
  | Ok f ->
      Printf.printf "File read successfully: %s\n" (File.get_name f);
      Printf.printf "File contents: %s\n" (File.get_content f ~format:`String)
  | Error e -> Printf.printf "%s\n" (Error.to_string e)

let () =
  (* Read to string *)
  match File.read_to_string "my-test.txt" with
  | Ok s -> Printf.printf "File read successfully: %s\n" s
  | Error e -> Printf.printf "Error reading file: %s\n" (Error.to_string e)

let () =
  (* Create *)
  match File.create "my-test.txt" () with
  | Ok () -> Printf.printf "File created successfully\n"
  | Error e -> Printf.printf "Error creating file: %s\n" (Error.to_string e)

let () =
  (* Delete *)
  match File.delete "my-test.txt" with
  | Ok () -> Printf.printf "File deleted successfully\n"
  | Error e -> Printf.printf "Error deleting file: %s" (Error.to_string e)
