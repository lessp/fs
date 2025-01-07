open Fs

(* Because the standard library has the arguments backwards *)
module R = struct
  let bind f = function
    | Ok x -> f x
    | Error e -> Error e
  ;;
end

let config_path = "./examples/config.json"

let update_config () =
  let default_config =
    {|
      {
        "theme": "dark",
        "fontSize": 14,
        "autoSave": true
      }
    |}
  in

  let updated_config =
    {|
      {
        "theme": "light",
        "fontSize": 16,
        "autoSave": true
      }
    |}
  in

  File.exists config_path
  |> R.bind (function
    | true -> Ok ()
    | false -> File.write_string config_path ~content:default_config)
  |> R.bind (fun () -> File.read_string config_path)
  |> R.bind (fun config ->
    Printf.printf "Current config:\n%s\n" config;
    File.write_string "config.json" ~content:updated_config)
;;

let () =
  match update_config () with
  | Ok () -> print_endline "Config updated successfully"
  | Error e -> Printf.printf "Error: %s\n" (File.Error.to_string e)
;;
