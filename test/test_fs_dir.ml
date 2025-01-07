open Alcotest
open Fs

let test_dir_path = "test_dir"
let nested_dir_path = "test_dir/nested"
let test_file_in_dir = "test_dir/file.txt"

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
    Dir.delete test_dir_path ~recursive:true () |> Result.get_ok;
    Alcotest.failf "Failed to delete directory: %s" (Dir.Error.to_string e)
;;

let test_delete_dir_if_exists () =
  let dir_path = "test_delete_dir" in
  Dir.create dir_path () |> Result.get_ok;

  match Dir.delete_if_exists dir_path () with
  | Ok `Directory_deleted ->
    Alcotest.(check bool)
      "Directory should not exist after deletion"
      false
      (Dir.exists dir_path |> Result.get_ok)
  | Ok `Directory_not_found ->
    Alcotest.fail "Directory was expected to exist but was not found."
  | Error e -> Alcotest.fail (Dir.Error.to_string e)
;;

let test_delete_dir_if_not_exists () =
  let dir_path = "nonexistent_dir" in
  match Dir.delete_if_exists dir_path () with
  | Ok `Directory_not_found ->
    Alcotest.(check bool) "Directory should not exist" true true
  | Ok `Directory_deleted ->
    Alcotest.fail "Directory was expected to not exist but was found."
  | Error e -> Alcotest.fail (Dir.Error.to_string e)
;;

let () =
  run
    "Fs Directory Tests"
    [ ( "directory"
      , [ test_case "Create directory" `Quick test_dir_create
        ; test_case "List directory contents" `Quick test_dir_list
        ; test_case "Delete directory" `Quick test_dir_delete
        ; test_case "Delete directory if exists" `Quick test_delete_dir_if_exists
        ; test_case "Delete directory if not exists" `Quick test_delete_dir_if_not_exists
        ] )
    ]
;;
