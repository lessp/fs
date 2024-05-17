module Error = struct
  type common_error =
    [ `File_not_found of string | `File_already_exists of string ]

  type read_error = [ `Error_reading_file of string ]
  type write_error = [ `Error_writing_to_file of string ]
  type t = [ read_error | write_error | common_error ]

  let to_string e =
    match e with
    | `Error_reading_file file -> Printf.sprintf "Error reading file: %s" file
    | `Error_writing_to_file file ->
        Printf.sprintf "Error writing to file: %s" file
    | `File_not_found file -> Printf.sprintf "File not found: %s" file
    | `File_already_exists file -> Printf.sprintf "File already exists: %s" file
end

module File = struct
  type readonly
  type writeable

  type 'mode t = {
    name : string;
    content : string;
    handle : [ `Ic of in_channel | `Oc of out_channel ] option;
  }

  let get_name { name; _ } = name
  let get_content { content; _ } format = match format with `String -> content

  let read file =
    try
      let content = In_channel.with_open_bin file In_channel.input_all in
      Ok { content; name = file; handle = None }
    with _exn -> Error (`Error_reading_file file)

  let read_as_string file =
    match read file with Ok { content; _ } -> Ok content | Error e -> Error e

  let write name ~contents =
    try
      Ok
        (Out_channel.with_open_bin name (fun oc ->
             match contents with `String s -> Out_channel.output_string oc s))
    with _exn -> Error (`Error_writing_to_file name)

  let open_file file ~mode =
    match mode with
    | `Readonly ->
        Ok
          {
            name = file;
            handle = Some (`Ic (In_channel.open_gen [ Open_rdonly ] 755 file));
            content = "";
          }
    | `Writeable ->
        Ok
          {
            name = file;
            content = "";
            handle =
              Some
                (`Oc
                  (Out_channel.open_gen
                     [ Open_wronly; Open_append; Open_creat ]
                     755 file));
          }
end [@warning "-69"]
