module File = struct
  module Error = struct
    type file_not_found = [ `File_not_found of string ]
    type file_already_exists = [ `File_already_exists of string ]
    type read_error = [ `Error_reading_file of string ]
    type write_error = [ `Error_writing_to_file of string ]
    type t =
      [ read_error
      | write_error
      | file_not_found
      | file_already_exists
      ]

    let to_string e =
      match e with
      | `Error_reading_file file -> Printf.sprintf "Error reading file: %s" file
      | `Error_writing_to_file file -> Printf.sprintf "Error writing to file: %s" file
      | `File_not_found file -> Printf.sprintf "File not found: %s" file
      | `File_already_exists file -> Printf.sprintf "File already exists: %s" file
    ;;
  end

  type _ format =
    | String : string format
    | Bytes : bytes format
    | Char : char option format
    | Byte : int option format
    | Lines : string list format

  type 'a t =
    { name : string
    ; content : 'a
    }

  let get_name { name; _ } = name
  let get_content { content; _ } = content

  let read
    : type a.
      string
      -> format:a format
      -> (a t, [> Error.read_error | Error.file_not_found ]) result
    =
    fun file ~format ->
    try
      match format with
      | String ->
        let content = In_channel.with_open_bin file In_channel.input_all in
        Ok { name = file; content }
      | Bytes ->
        let content =
          In_channel.with_open_bin file In_channel.input_all |> Bytes.of_string
        in
        Ok { name = file; content }
      | Char ->
        let content = In_channel.with_open_text file In_channel.input_char in
        Ok { name = file; content }
      | Byte ->
        let content = In_channel.with_open_bin file In_channel.input_byte in
        Ok { name = file; content }
      | Lines ->
        let content = In_channel.with_open_text file In_channel.input_lines in
        Ok { name = file; content }
    with
    | _ -> Error (`Error_reading_file file)
  ;;

  let read_to_string file =
    match read file ~format:String with
    | Ok { content; _ } -> Ok content
    | Error e -> Error e
  ;;

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
    with
    | _exn -> Error (`Error_writing_to_file name)
  ;;

  let exists name =
    match Bos.OS.File.exists (Fpath.v name) with
    | Ok exists -> Ok exists
    | Error (`Msg msg) -> Error (`Error_reading_file msg)
  ;;

  let create name ?(contents = `String "") ?(overwrite = false) () =
    if (not overwrite) && Sys.file_exists name then
      Error (`File_already_exists name)
    else
      write name ~contents
  ;;

  let delete name =
    if Sys.file_exists name then
      Ok (Sys.remove name)
    else
      Error (`File_not_found name)
  ;;
end

module Dir = struct
  module Error = struct
    type read_error = [ `Error_reading_directory of string ]
    type write_error = [ `Error_creating_directory of string ]
    type dir_not_found = [ `Directory_not_found of string ]
    type dir_already_exists = [ `Directory_already_exists of string ]
    type dir_not_empty = [ `Directory_not_empty of string ]

    type t =
      [ read_error
      | write_error
      | dir_not_found
      | dir_already_exists
      | dir_not_empty
      ]

    let to_string = function
      | `Error_reading_directory dir -> Printf.sprintf "Error reading directory: %s" dir
      | `Error_creating_directory dir -> Printf.sprintf "Error creating directory: %s" dir
      | `Directory_not_found dir -> Printf.sprintf "Directory not found: %s" dir
      | `Directory_already_exists dir -> Printf.sprintf "Directory already exists: %s" dir
      | `Directory_not_empty dir -> Printf.sprintf "Directory not empty: %s" dir
    ;;
  end

  type t = { path : Fpath.t }
  type entry =
    | File : 'a File.t -> entry
    | Directory of t

  let get_name { path } = Fpath.basename path

  let exists name =
    match Bos.OS.Dir.exists (Fpath.v name) with
    | Ok exists -> Ok exists
    | Error (`Msg msg) -> Error (`Error_reading_directory msg)
  ;;

  let create name ?(recursive = false) ?(mode = 0o777) () =
    let path = Fpath.v name in
    match Bos.OS.Dir.create ~path:recursive ~mode path with
    (* Dir did not exist, so we created it *)
    | Ok true -> Ok ()
    (* Dir did exist, it's possible that it's a symlink. It's kept as is. *)
    | Ok false -> Ok ()
    | Error (`Msg _) -> Error (`Error_creating_directory name)
  ;;

  let list path ?(dotfiles = false) () =
    let dir_path = Fpath.v path in
    match exists path with
    | Error e -> Error e
    | Ok false -> Error (`Directory_not_found path)
    | Ok true ->
      (match Bos.OS.Dir.contents ~dotfiles dir_path with
       | Ok entries ->
         (try
            let entries_with_types =
              List.map
                (fun entry ->
                  match Bos.OS.Dir.exists entry with
                  | Ok true -> Directory { path = entry }
                  | Ok false ->
                    (* It's a file - just use the basename for name *)
                    File { name = Fpath.basename entry; content = "" }
                  | Error (`Msg msg) -> raise (Failure msg))
                entries
            in
            Ok entries_with_types
          with
          | Failure msg -> Error (`Error_reading_directory msg))
       | Error (`Msg msg) -> Error (`Error_reading_directory msg))
  ;;

  let delete name ?(must_exist = false) ?(recursive = false) () =
    match Bos.OS.Dir.delete ~must_exist ~recurse:recursive (Fpath.v name) with
    | Ok () -> Ok ()
    | Error (`Msg msg) -> Error (`Error_reading_directory msg)
  ;;

  module Entry = struct
    let get_name = function
      | File f -> File.get_name f
      | Directory d -> get_name d
    ;;

    let is_file = function
      | File _ -> true
      | Directory _ -> false
    ;;
    let is_directory = function
      | File _ -> false
      | Directory _ -> true
    ;;
  end
end
