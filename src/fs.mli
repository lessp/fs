module File : sig
  module Error : sig
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

    val to_string : t -> string
  end

  type _ format =
    | String : string format
    | Bytes : bytes format
    | Char : char option format
    | Byte : int option format
    | Lines : string list format

  type 'a t

  (** Returns the name of the file.

      Examples:

      {[
        let file = File.create "file.txt" () |> Result.get_ok in
        print_endline (File.get_name file)
      ]} *)
  val get_name : 'a t -> string

  (** Returns the contents of the file.

      Examples:

      {[
        let file =
          File.create "file.txt" ~contents:(`String "Hello, World!") () |> Result.get_ok
        in
        print_endline (File.get_content file ~format:`String)
      ]} *)
  val get_content : 'a t -> 'a

  (** Reads the contents of a file. After reading, the file is closed.

      Examples:

      {[
        match File.read "file.txt" ~format:`String with
        | Ok file -> print_endline (File.get_content file `String)
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val read
    :  string
    -> format:'a format
    -> ('a t, [> Error.read_error | Error.file_not_found ]) result

  (** Reads the contents of a file as a string. After reading the file is closed.

      Examples:

      {[
        match File.read_to_string "file.txt" with
        | Ok content -> print_endline content
        | Error e -> print_endline (File.Error.to_string e)
      ]}

      {[
        let contents = File.read_as_string "file.txt" |> Result.get_ok in
        print_endline contents
      ]} *)
  val read_to_string
    :  string
    -> (string, [> Error.read_error | Error.file_not_found ]) result

  (** Writes content to a file. After writing the file is closed.

      Examples:

      {[
        match File.write "file.txt" ~contents:(`String "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write
    :  string
    -> contents:
         [ `String of string
         | `Bytes of bytes
         | `Char of char
         | `Byte of int
         | `Substring of string * int * int
         | `Bigarray of
           (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
           * int
           * int
         ]
    -> (unit, [> Error.write_error | Error.file_not_found ]) result

  (** Creates a new file.

      Examples:

      {[
        match File.create "file.txt" () with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (File.Error.to_string e)
      ]}

      {[
        match
          File.create "file.txt" ~contents:(`String "Hello, world!") ~overwrite:true ()
        with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val create
    :  string
    -> ?contents:
         [ `String of string
         | `Bytes of bytes
         | `Char of char
         | `Byte of int
         | `Substring of string * int * int
         | `Bigarray of
           (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
           * int
           * int
         ]
    -> ?overwrite:bool
    -> unit
    -> ( unit
         , [> Error.write_error | Error.file_not_found | Error.file_already_exists ] )
         result

  (** Deletes a file.

      Examples:

      {[
        match File.delete "file.txt" with
        | Ok () -> print_endline "File deleted successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val delete : string -> (unit, [> Error.file_not_found ]) result

  (** Checks if a file exists.

      Examples:

      {[
        match File.exists "file.txt" with
        | Ok true -> print_endline "File exists"
        | Ok false -> print_endline "File does not exist"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val exists : string -> (bool, [> Error.read_error ]) result
end

module Dir : sig
  module Error : sig
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

    val to_string : t -> string
  end

  type t
  type entry =
    | File : 'a File.t -> entry
    | Directory of t

  (** Returns the name of the directory.

      Examples:

      {[
        let dir = Dir.create "mydir" () |> Result.get_ok in
        print_endline (Dir.get_name dir)
      ]} *)
  val get_name : t -> string

  (** Creates a new directory. If recursive is true, creates parent directories
      if they don't exist. If the directory already exists it's left unchanged.

      Default mode is 0o755.

      Examples:

      {[
        match Dir.create "mydir" () with
        | Ok dir -> print_endline (Dir.get_name dir)
        | Error e -> print_endline (Dir.Error.to_string e)
      ]}

      {[
        (* Create nested directories *)
        match Dir.create "path/to/mydir" ~recursive:true () with
        | Ok dir -> print_endline (Dir.get_name dir)
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val create
    :  string
    -> ?recursive:bool
    -> ?mode:int
    -> unit
    -> (unit, [> Error.write_error ]) result

  (* val read : string -> (t, [> Error.read_error | Error.dir_not_found ]) result *)
  (** Opens and reads a directory.

      Examples:

      {[
        match Dir.read "mydir" with
        | Ok dir ->
          List.iter (fun entry -> print_endline (Dir.Entry.get_name entry)) (Dir.list dir)
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)

  (** Lists all entries in the directory.

      Examples:

      {[
        match Dir.list "mydir" () with
        | Ok entries ->
          List.iter
            (function
              | Dir.File file -> print_endline (File.get_name file)
              | Dir.Directory dir -> print_endline (Dir.get_name dir))
            entries
        | Error e -> print_endline (Dir.Error.to_string e)
      ]}

      {[
        (* List dotfiles *)
        match Dir.list "mydir" ~dotfiles:true () with
        | Ok entries ->
          List.iter
            (function
              | Dir.File file -> print_endline (File.get_name file)
              | Dir.Directory dir -> print_endline (Dir.get_name dir))
            entries
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val list
    :  string
    -> ?dotfiles:bool
    -> unit
    -> (entry list, [> Error.dir_not_found | Error.read_error ]) result

  (** Deletes a directory. If recursive is true, deletes all contents recursively.

      Examples:

      {[
        match Dir.delete "mydir" () with
        | Ok () -> print_endline "Directory deleted"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]}

      {[
        (* Delete directory and all contents *)
        match Dir.delete "mydir" ~recursive:true () with
        | Ok () -> print_endline "Directory and contents deleted"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val delete
    :  string
    -> ?must_exist:bool
    -> ?recursive:bool
    -> unit
    -> (unit, [> Error.read_error ]) result

  (** Checks if a directory exists.

      Examples:

      {[
        match Dir.exists "mydir" with
        | Ok true -> print_endline "Directory exists"
        | Ok false -> print_endline "Directory does not exist"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val exists : string -> (bool, [> Error.read_error ]) result

  module Entry : sig
    (** Gets the name of a directory entry, whether file or directory *)
    val get_name : entry -> string

    (** Returns true if the entry is a file *)
    val is_file : entry -> bool

    (** Returns true if the entry is a directory *)
    val is_directory : entry -> bool
  end
end
