open Fs

let temp_file_path = "test_file"
let binary_file_path = "test_binary"
let test_dir_path = "test_dir"
let nested_dir_path = "test_dir/nested"
let test_file_in_dir = "test_dir/file.txt"

let setup_files () =
  File.write_string temp_file_path ~content:"hello\n" |> Result.get_ok;
  File.write_bytes binary_file_path ~content:(Bytes.of_string "\001\002\003")
  |> Result.get_ok
;;

let cleanup_files () =
  File.delete temp_file_path |> Result.get_ok;
  File.delete binary_file_path |> Result.get_ok
;;

let test_read_string () =
  match File.read_string temp_file_path with
  | Ok content -> Alcotest.(check string) "same string" "hello\n" content
  | Error e -> Alcotest.fail (File.Error.to_string e)
;;

let test_read_bytes () =
  match File.read_bytes binary_file_path with
  | Ok content ->
    Alcotest.(check int) "correct length" 3 (Bytes.length content);
    Alcotest.(check bytes) "same bytes" (Bytes.of_string "\001\002\003") content
  | Error e -> Alcotest.fail (File.Error.to_string e)
;;

let test_read_nonexistent () =
  match File.read_string "nonexistent.txt" with
  | Ok content ->
    Alcotest.failf "Should not succeed reading non-existent file. Got content: %S" content
  | Error e ->
    Printf.printf "Received error: %s\n" (File.Error.to_string e);
    let is_error_reading =
      match e with
      | `Error_reading_file path ->
        Printf.printf "Error reading file for path: %s\n" path;
        true
      | other ->
        Printf.printf "Unexpected error variant: %s\n" (File.Error.to_string other);
        false
    in
    Alcotest.(check bool) "is error reading file" true is_error_reading
;;

let setup_test_dir () =
  Dir.create test_dir_path () |> Result.get_ok;
  Dir.create nested_dir_path ~recursive:true () |> Result.get_ok;
  File.write_string test_file_in_dir ~content:"test content\n" |> Result.get_ok
;;

let cleanup_test_dir () = Dir.delete test_dir_path ~recursive:true () |> Result.get_ok

let test_dir_create () =
  let dir_name = "new_test_dir" in
  match Dir.create dir_name () with
  | Ok () ->
    let exists = Dir.exists dir_name |> Result.get_ok in
    Dir.delete dir_name () |> Result.get_ok;
    Alcotest.(check bool) "directory was created" true exists
  | Error e -> Alcotest.failf "Failed to create directory: %s" (Dir.Error.to_string e)
;;

let test_dir_list () =
  setup_test_dir ();
  match Dir.list test_dir_path () with
  | Ok entries ->
    let has_file =
      List.exists Dir.Entry.(fun e -> is_file e && get_name e = "file.txt") entries
    in
    let has_dir =
      List.exists Dir.Entry.(fun e -> is_directory e && get_name e = "nested") entries
    in
    cleanup_test_dir ();
    Alcotest.(check bool) "has expected file" true has_file;
    Alcotest.(check bool) "has expected directory" true has_dir
  | Error e ->
    cleanup_test_dir ();
    Alcotest.failf "Failed to list directory: %s" (Dir.Error.to_string e)
;;

let test_dir_delete () =
  Dir.create test_dir_path () |> Result.get_ok;
  match Dir.delete test_dir_path () with
  | Ok () ->
    let exists = Dir.exists test_dir_path |> Result.get_ok in
    Alcotest.(check bool) "directory was deleted" false exists
  | Error e ->
    Dir.delete test_dir_path ~recursive:true () |> ignore;
    Alcotest.failf "Failed to delete directory: %s" (Dir.Error.to_string e)
;;

let () =
  let open Alcotest in
  run
    "Fs"
    [ ( "file"
      , [ test_case "Read string file" `Quick (fun () ->
            setup_files ();
            test_read_string ();
            cleanup_files ())
        ; test_case "Read binary file" `Quick (fun () ->
            setup_files ();
            test_read_bytes ();
            cleanup_files ())
        ; test_case "Read non-existent file" `Quick test_read_nonexistent
        ] )
    ; ( "directory"
      , [ test_case "Create directory" `Quick test_dir_create
        ; test_case "List directory contents" `Quick test_dir_list
        ; test_case "Delete directory" `Quick test_dir_delete
        ] )
    ]
;;
