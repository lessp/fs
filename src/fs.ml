module Error = struct
  type file_not_found = [ `File_not_found of string ]
  type file_already_exists = [ `File_already_exists of string ]
  type read_error = [ `Error_reading_file of string ]
  type write_error = [ `Error_writing_to_file of string ]
  type t = [ read_error | write_error | file_not_found | file_already_exists ]

  let to_string e =
    match e with
    | `Error_reading_file file -> Printf.sprintf "Error reading file: %s" file
    | `Error_writing_to_file file ->
        Printf.sprintf "Error writing to file: %s" file
    | `File_not_found file -> Printf.sprintf "File not found: %s" file
    | `File_already_exists file -> Printf.sprintf "File already exists: %s" file
end

module File = struct
  type 'content t = { name : string; content : 'content }

  let get_name { name; _ } = name
  let get_content { content; _ } format = match format with `String -> content

  let read file =
    try
      let content = In_channel.with_open_bin file In_channel.input_all in
      Ok { content; name = file }
    with _exn -> Error (`Error_reading_file file)

  let read_to_string file =
    match read file with Ok { content; _ } -> Ok content | Error e -> Error e

  let write name ~contents =
    try
      Ok
        (Out_channel.with_open_bin name (fun oc ->
             match contents with
             | `String s -> Out_channel.output_string oc s
             | `Bytes b -> Out_channel.output_bytes oc b
             | `Char c -> Out_channel.output_char oc c
             | `Byte i -> Out_channel.output_byte oc i
             | `Substring (s, i, j) -> Out_channel.output_substring oc s i j
             | `Bigarray (a, i, j) -> Out_channel.output_bigarray oc a i j))
    with _exn -> Error (`Error_writing_to_file name)

  let create name ?(contents = `String "") ?(overwrite = false) () =
    if (not overwrite) && Sys.file_exists name then
      Error (`File_already_exists name)
    else write name ~contents

  let delete name =
    if Sys.file_exists name then Ok (Sys.remove name)
    else Error (`File_not_found name)
end
