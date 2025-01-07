let () =
  let open Alcotest in
  run "Fs Tests"
    [ ( "File Tests"
      , [ test_case "File Tests" `Quick (fun () ->
            let _ = Sys.command "dune exec ./test_fs_file.exe" in
            () ) ] )
    ; ( "Directory Tests"
      , [ test_case "Directory Tests" `Quick (fun () ->
            let _ = Sys.command "dune exec ./test_fs_dir.exe" in
            () ) ] )
    ]
;; 