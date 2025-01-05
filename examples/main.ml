(* Write *)
match Fs.File.write "my-test.txt" ~contents:(`String "Hello, world!") with
| Ok () -> Printf.printf "File written successfully\n"
| Error e -> Printf.printf "Error writing file: %s" (Fs.Error.to_string e)
;;

(* Read - Fail *)
match Fs.File.read "my-test-does-not-exist.txt" with
| Ok f -> Printf.printf "File read successfully: %s\n" (Fs.File.get_name f)
| Error e -> Printf.printf "%s\n" (Fs.Error.to_string e)
;;

(* Read - Success *)
match Fs.File.read "my-test.txt" with
| Ok f ->
    Printf.printf "File read successfully: %s\n" (Fs.File.get_name f);
    Printf.printf "File contents: %s\n" (Fs.File.get_content f `String)
| Error e -> Printf.printf "%s\n" (Fs.Error.to_string e)
;;

(* Read to string *)
match Fs.File.read_to_string "my-test.txt" with
| Ok s -> Printf.printf "File read successfully: %s\n" s
| Error e -> Printf.printf "Error reading file: %s\n" (Fs.Error.to_string e)
;;

(* Create *)
match Fs.File.create "my-test.txt" () with
| Ok () -> Printf.printf "File created successfully\n"
| Error e -> Printf.printf "Error creating file: %s\n" (Fs.Error.to_string e)
;;

(* Delete *)
match Fs.File.delete "my-test.txt" with
| Ok () -> Printf.printf "File deleted successfully\n"
| Error e -> Printf.printf "Error deleting file: %s" (Fs.Error.to_string e)

(* Misc *)
