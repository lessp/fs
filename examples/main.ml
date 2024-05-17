(* Write *)
match Fs.File.write "my-test.txt" ~contents:(`String "Hello, world!") with
| Ok () -> Printf.printf "File written successfully\n"
| Error e -> Printf.printf "Error writing file: %s" (Fs.Error.to_string e)
;;

(* Read *)
match Fs.File.read "my-test.txt" with
| Ok f -> Printf.printf "File read successfully: %s\n" (Fs.File.get_name f)
| Error e -> Printf.printf "%s" (Fs.Error.to_string e)
;;

(* Read as string *)
match Fs.File.read_as_string "my-test.txt" with
| Ok s -> Printf.printf "File read successfully: %s\n" s
| Error e -> Printf.printf "Error reading file: %s\n" (Fs.Error.to_string e)
;;

match Fs.File.open_file "my-test.txt" ~mode:`Readonly with
| Ok f -> Printf.printf "File opened successfully: %s\n" (Fs.File.get_name f)
| Error e -> Printf.printf "Error opening file: %s\n" (Fs.Error.to_string e)

(* Append *)
