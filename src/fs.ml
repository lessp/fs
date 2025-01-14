module File = struct
  module Error = struct
    type file_not_found = [ `FileNotFound of string ]
    type file_already_exists = [ `FileAlreadyExists of string ]
    type read_error = [ `FileReadError of string ]
    type write_error = [ `FileWriteError of string ]
    type delete_error = [ `FileDeleteError of string ]

    type t =
      [ read_error
      | delete_error
      | file_already_exists
      | file_not_found
      | write_error
      ]

    let to_string = function
      | `FileReadError msg -> Printf.sprintf "Error reading file: %s" msg
      | `FileWriteError msg -> Printf.sprintf "Error writing to file: %s" msg
      | `FileDeleteError msg -> Printf.sprintf "Error deleting file: %s" msg
      | `FileAlreadyExists msg -> Printf.sprintf "File already exists: %s" msg
      | `FileNotFound msg -> Printf.sprintf "File not found: %s" msg
    ;;
  end

  type _ format =
    | String : string format
    | Bytes : bytes format
    | Char : char option format
    | Byte : int option format
    | Lines : string list format

  type content =
    | String of string
    | Bytes of bytes
    | Char of char
    | Byte of int
    | Substring of string * int * int
    | Bigarray of
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        * int
        * int

  type 'format t =
    { name : string
    ; content : 'format
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
          In_channel.with_open_bin file (fun ic ->
            let length = Int64.to_int (In_channel.length ic) in
            let buffer = Bytes.create length in
            let actually_read = In_channel.really_input ic buffer 0 length in
            match actually_read with
            | Some () -> buffer
            | None -> buffer)
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
    | Sys_error msg -> Error (`FileReadError msg)
    | _ -> Error (`FileReadError file)
  ;;

  let read_bytes file =
    match read file ~format:Bytes with
    | Ok { content; _ } -> Ok content
    | Error e -> Error e
  ;;

  let read_lines file =
    match read file ~format:Lines with
    | Ok { content; _ } -> Ok content
    | Error e -> Error e
  ;;

  let read_string file =
    match read file ~format:String with
    | Ok { content; _ } -> Ok content
    | Error e -> Error e
  ;;

  let write name ~content ~append () =
    let flags = [ Open_wronly; Open_creat; Open_binary ] in
    let flags =
      if append then
        Open_append :: flags
      else
        Open_trunc :: flags
    in
    try
      Ok
        (Out_channel.with_open_gen flags 0o666 name (fun oc ->
           match content with
           | String s -> Out_channel.output_string oc s
           | Bytes b -> Out_channel.output_bytes oc b
           | Char c -> Out_channel.output_char oc c
           | Byte i -> Out_channel.output_byte oc i
           | Substring (s, i, j) -> Out_channel.output_substring oc s i j
           | Bigarray (a, i, j) -> Out_channel.output_bigarray oc a i j))
    with
    | _exn -> Error (`FileWriteError name)
  ;;

  let append name ~content = write name ~content ~append:true ()

  let write_string name ~content = write name ~content:(String content) ~append:false ()
  let write_byte name ~content = write name ~content:(Byte content) ~append:false ()
  let write_bytes name ~content = write name ~content:(Bytes content) ~append:false ()
  let write_char name ~content = write name ~content:(Char content) ~append:false ()
  let write_lines name content =
    write name ~content:(String (String.concat "\n" content)) ~append:false ()
  ;;

  let append_string name ~content = append name ~content:(String content)
  let append_byte name ~content = append name ~content:(Byte content)
  let append_bytes name ~content = append name ~content:(Bytes content)
  let append_char name ~content = append name ~content:(Char content)
  let append_lines name content =
    append name ~content:(String (String.concat "\n" content))
  ;;

  (*(1** Creates a temporary file for the duration of the function. The file is deleted after the function returns. *)

  (*    Examples: *)

  (*    {[ *)
  (*      File.with_temp (fun file -> *)
  (*        File.write_string file ~content:"Hello, World!" *)
  (*        |> Result.map_error (fun e -> Error.to_string e) *)
  (*        |> Result.get_ok) *)
  (*    ]} *1) *)
  (*val with_temp : (string -> ('a, [> Error.t ]) result) -> ('a, [> Error.t ]) result *)

  (* (1* Create a temporary file with some content *1) *)
  (* let write_temp_data data = *)
  (*   let pat = "temp-%s.txt" in  (1* Pattern for temp filename *1) *)
  (*   Bos.OS.File.with_tmp_output *)
  (*     ~mode:0o600               (1* File permissions: user read/write only *1) *)
  (*     ~dir:(Fpath.v "/tmp")     (1* Optional: specify directory *1) *)
  (*     pat                       (1* Filename pattern *1) *)
  (*     (fun path output data ->  (1* Function that uses the temp file *1) *)
  (*       Bos.OS.File.write_lines path [data]) *)
  (*     data                      (1* Data to pass to the function *1) *)

  (* Usage example *)
  (* let () = *)
  (*   match write_temp_data "Hello, temporary file!" with *)
  (*   | Ok () -> Printf.printf "Successfully wrote to temp file\n" *)
  (*   | Error (`Msg e) -> Printf.printf "Error: %s\n" e *)

  (* let with_temp f = *)
  (*   Bos.OS.File.with_tmp_output "temp-%s.txt" (fun path output -> f path output) *)
  (* ;; *)

  let exists name =
    match Bos.OS.File.exists (Fpath.v name) with
    | Ok exists -> Ok exists
    | Error (`Msg msg) -> Error (`FileReadError msg)
  ;;

  let create name ?(content = String "") ?(overwrite = false) () =
    match overwrite, exists name with
    | false, Ok true -> Error (`FileAlreadyExists name)
    | true, Ok false | true, Ok true | false, Ok false ->
      write name ~content ~append:false ()
    | _, Error e -> Error e
  ;;

  let delete name =
    match Bos.OS.File.delete (Fpath.v name) with
    | Ok () -> Ok ()
    | Error (`Msg msg) -> Error (`FileDeleteError msg)
  ;;

  let delete_if_exists name =
    match exists name with
    | Ok true -> delete name |> Result.map (fun _ -> `FileDeleted)
    | Ok false -> Ok `FileNotFound
    | Error e -> Error e
  ;;
end

module Dir = struct
  module Error = struct
    type read_error = [ `DirectoryReadError of string ]
    type delete_error = [ `DirectoryDeleteError of string ]
    type write_error = [ `DirectoryWriteError of string ]
    type dir_not_found = [ `DirectoryNotFound of string ]
    type dir_already_exists = [ `DirectoryAlreadyExists of string ]
    type dir_not_empty = [ `DirectoryNotEmpty of string ]

    type t =
      [ read_error
      | write_error
      | delete_error
      | dir_not_found
      | dir_already_exists
      | dir_not_empty
      ]

    let to_string = function
      | `DirectoryReadError msg -> Printf.sprintf "Error reading directory: %s"
      msg
      | `DirectoryDeleteError msg -> Printf.sprintf "Error deleting directory:
        %s"
      msg
      | `DirectoryWriteError msg -> Printf.sprintf "Error writing to directory:
        %s"
      msg
      | `DirectoryNotFound msg -> Printf.sprintf "Directory not found: %s" msg
      | `DirectoryAlreadyExists msg -> Printf.sprintf "Directory already exists:
        %s"
      msg
      | `DirectoryNotEmpty msg -> Printf.sprintf "Directory not empty: %s" msg


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
    | Error (`Msg msg) -> Error (`DirectoryReadError msg)
  ;;

  let create name ?(recursive = false) ?(mode = 0o777) () =
    let path = Fpath.v name in
    match Bos.OS.Dir.create ~path:recursive ~mode path with
    (* Dir did not exist, so we created it *)
    | Ok true -> Ok ()
    (* Dir did exist, it's possible that it's a symlink. It's kept as is. *)
    | Ok false -> Ok ()
    | Error (`Msg msg) -> Error (`DirectoryWriteError msg)
  ;;

  let list path ?(dotfiles = false) () =
    let dir_path = Fpath.v path in
    match exists path with
    | Error e -> Error e
    | Ok false -> Error (`DirectoryNotFound path)
    | Ok true ->
      (match Bos.OS.Dir.contents ~dotfiles dir_path with
       | Ok entries ->
         let entries_with_types =
           List.map
             (fun entry ->
               match Bos.OS.Dir.exists entry with
               | Ok true -> Directory { path = entry }
               | Ok false -> File { name = Fpath.basename entry; content = "" }
               | Error (`Msg msg) -> raise (Failure msg))
             entries
         in
         Ok entries_with_types
       | Error (`Msg msg) -> Error (`DirectoryReadError msg))
  ;;

  let delete name ?(must_exist = false) ?(recursive = false) () =
    match Bos.OS.Dir.delete ~must_exist ~recurse:recursive (Fpath.v name) with
    | Ok () -> Ok ()
    | Error (`Msg msg) -> Error (`DirectoryDeleteError msg)
  ;;

  let delete_if_exists name ?(recursive = false) () =
    match exists name with
    | Ok true -> delete name ~recursive () |> Result.map (fun _ -> `DirectoryDeleted)
    | Ok false -> Ok `DirectoryNotFound
    | Error e -> Error e
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
