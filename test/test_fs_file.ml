open Alcotest
open Fs

let temp_file_path = "test_file"
let binary_file_path = "test_binary"

let setup_files () =
  File.write_string temp_file_path ~content:"hello\n\n" |> Result.get_ok;
  File.write_bytes binary_file_path ~content:(Bytes.of_string "\001\002\003")
  |> Result.get_ok
;;

let cleanup_files () =
  File.delete temp_file_path |> Result.get_ok;
  File.delete binary_file_path |> Result.get_ok
;;

let test_read_string () =
  match File.read_string temp_file_path with
  | Ok content -> Alcotest.(check string) "same string" "hello\n\n" content
  | Error e -> Alcotest.fail (File.Error.to_string e)
;;

let test_read_lines () =
  match File.read_lines temp_file_path with
  | Ok lines ->
    Alcotest.(check int) "correct length" 2 (List.length lines);
    Alcotest.(check string) "same line" "hello" (List.hd lines)
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
    Alcotest.(check bool)
      "is error reading file"
      true
      (match e with
       | `Error_reading_file _ -> true
       | _ -> false)
;;

let test_delete_if_exists () =
  let file_path = "temp_file.txt" in
  (* Create a file to test deletion *)
  File.write_string file_path ~content:"This is a test file." |> Result.get_ok;

  match File.delete_if_exists file_path with
  | Ok `File_deleted ->
    Alcotest.(check bool)
      "File should not exist after deletion"
      false
      (File.exists file_path |> Result.get_ok)
  | Ok `File_not_found -> Alcotest.fail "File was expected to exist but was not found."
  | Error e -> Alcotest.fail (File.Error.to_string e)
;;

let test_delete_if_not_exists () =
  let file_path = "nonexistent_file.txt" in
  match File.delete_if_exists file_path with
  | Ok `File_not_found -> Alcotest.(check bool) "File should not exist" true true
  | Ok `File_deleted -> Alcotest.fail "File was expected to not exist but was found."
  | Error e -> Alcotest.fail (File.Error.to_string e)
;;

let () =
  run
    "Fs File Tests"
    [ ( "file"
      , [ test_case "Read string file" `Quick (fun () ->
            setup_files ();
            test_read_string ();
            cleanup_files ())
        ; test_case "Read lines file" `Quick (fun () ->
            setup_files ();
            test_read_lines ();
            cleanup_files ())
        ; test_case "Read binary file" `Quick (fun () ->
            setup_files ();
            test_read_bytes ();
            cleanup_files ())
        ; test_case "Read non-existent file" `Quick test_read_nonexistent
        ; test_case "Delete if exists" `Quick test_delete_if_exists
        ; test_case "Delete if not exists" `Quick test_delete_if_not_exists
        ] )
    ]
;;
