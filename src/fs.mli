module File : sig
  module Error : sig
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

    val to_string : t -> string
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

  type 'format t

  (** Returns the name of the file.

      Examples:

      {[
        let file = File.create "file.txt" () |> Result.get_ok in
        print_endline (File.get_name file)
      ]} *)
  val get_name : 'format t -> string

  (** Returns the contents of the file.

      Examples:

      {[
        let file =
          File.create "file.txt" ~content:(String "Hello, World!") () |> Result.get_ok
        in
        print_endline (File.get_content file)
      ]} *)
  val get_content : 'format t -> 'format

  (** Reads the contents of a file. After reading, the file is closed.

      Examples:

      {[
        match File.read "file.txt" ~format:String with
        | Ok file -> print_endline (File.get_content file)
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val read
    :  string
    -> format:'format format
    -> ('format t, [> Error.read_error | Error.file_not_found ]) result

  (** Reads the contents of a file as bytes. After reading the file is closed.

      Examples:

      {[
        match File.read_bytes "file.txt" with
        | Ok bytes -> print_endline (Bytes.to_string (File.get_content bytes))
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val read_bytes : string -> (bytes, [> Error.read_error | Error.file_not_found ]) result

  (** Reads the contents of a file as lines.
      Newline characters that terminate lines are not included in the returned strings.  Empty lines produce empty strings.
      After reading the file is closed.

      Examples:

      {[
        match File.read_lines "file.txt" with
        | Ok lines -> List.iter print_endline lines
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val read_lines
    :  string
    -> (string list, [> Error.read_error | Error.file_not_found ]) result

  (** Reads the contents of a file as a string. After reading the file is closed.

      Examples:

      {[
        match File.read_string "file.txt" with
        | Ok content -> print_endline content
        | Error e -> print_endline (File.Error.to_string e)
      ]}

      {[
        let contents = File.read_string "file.txt" |> Result.get_ok in
        print_endline contents
      ]} *)
  val read_string
    :  string
    -> (string, [> Error.read_error | Error.file_not_found ]) result

  (** Writes content to a file. After writing the file is closed.

      Examples:

      {[
        match File.write "file.txt" ~content:(String "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write
    :  string
    -> content:content
    -> append:bool
    -> unit
    -> (unit, [> Error.write_error ]) result

  (** Writes contents to a file as a string. After writing the file is closed.

      Examples:

      {[
        match File.write_string "file.txt" ~content:"Hello, World!" with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write_string : string -> content:string -> (unit, [> Error.write_error ]) result

  (** Writes a single byte to a file. After writing the file is closed.

      Examples:

      {[
        match File.write_byte "file.txt" ~content:65 with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write_byte : string -> content:int -> (unit, [> Error.write_error ]) result

  (** Writes contents to a file as bytes. After writing the file is closed.

      Examples:

      {[
        match File.write_bytes "file.txt" ~content:(Bytes.of_string "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write_bytes : string -> content:bytes -> (unit, [> Error.write_error ]) result

  (** Writes a single character to a file. After writing the file is closed.

      Examples:

      {[
        match File.write_char "file.txt" ~content:'A' with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write_char : string -> content:char -> (unit, [> Error.write_error ]) result

  (** Write a list of strings to a file, joining them with newlines.
      Each line will be terminated with a single newline character (\n).
      The file is closed after writing.

      Examples:
      {[
        match File.write_lines "output.txt" [ "line1"; "line2"; "line3" ] with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val write_lines : string -> string list -> (unit, [> Error.write_error ]) result

  (** Appends content to a file. After writing the file is closed.

      Examples:

      {[
        match File.append "file.txt" ~content:(String "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append : string -> content:content -> (unit, [> Error.write_error ]) result

  (** Appends a string to a file. After writing the file is closed.

      Examples:

      {[
        match File.append_string "file.txt" ~content:"Hello, World!" with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append_string : string -> content:string -> (unit, [> Error.write_error ]) result

  (** Appends a single byte to a file. After writing the file is closed.

      Examples:

      {[
        match File.append_byte "file.txt" ~content:65 with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append_byte : string -> content:int -> (unit, [> Error.write_error ]) result

  (** Appends bytes to a file. After writing the file is closed.

      Examples:

      {[
        match File.append_bytes "file.txt" ~content:(Bytes.of_string "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append_bytes : string -> content:bytes -> (unit, [> Error.write_error ]) result

  (** Appends a single character to a file. After writing the file is closed.

      Examples:

      {[
        match File.append_char "file.txt" ~content:'A' with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append_char : string -> content:char -> (unit, [> Error.write_error ]) result

  (** Appends a list of strings to a file, joining them with newlines.
      Each line will be terminated with a single newline character (\n).
      The file is closed after writing.

      Examples:
      {[
        match File.append_lines "output.txt" [ "line1"; "line2"; "line3" ] with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val append_lines : string -> string list -> (unit, [> Error.write_error ]) result

  (** Creates a new file.

      Examples:

      {[
        match File.create "file.txt" () with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (File.Error.to_string e)
      ]}

      {[
        match
          File.create "file.txt" ~content:(String "Hello, world!") ~overwrite:true ()
        with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val create
    :  string
    -> ?content:content
    -> ?overwrite:bool
    -> unit
    -> ( unit
         , [> Error.read_error | Error.write_error | Error.file_already_exists ] )
         result

  (** Deletes a file if it exists.

      Examples:

      {[
        match File.delete_if_exists "file.txt" with
        | Ok `File_not_found -> print_endline "File does not exist"
        | Ok `File_deleted -> print_endline "File deleted"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val delete_if_exists
    :  string
    -> ( [ `FileNotFound | `FileDeleted ]
         , [> Error.read_error | Error.delete_error ] )
         result

  (** Deletes a file.

      Examples:

      {[
        match File.delete "file.txt" with
        | Ok () -> print_endline "File deleted successfully"
        | Error e -> print_endline (File.Error.to_string e)
      ]} *)
  val delete : string -> (unit, [> Error.delete_error ]) result

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

    val to_string : t -> string
  end

  type t
  type entry =
    | File : 'format File.t -> entry
    | Directory of t

  (** Returns the name of the directory. *)
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

  (** Deletes a directory if it exists. If recursive is true (defaults to false), deletes all contents recursively.

      Examples:

      {[
        match Dir.delete_if_exists "mydir" () with
        | Ok `DirectoryNotFound -> print_endline "Directory does not exist"
        | Ok `DirectoryDeleted -> print_endline "Directory deleted"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]}

      {[
        (* Delete directory and all contents *)
        match Dir.delete_if_exists "mydir" ~recursive:true () with
        | Ok _ -> print_endline "Directory and contents deleted or did not exist"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val delete_if_exists
    :  string
    -> ?recursive:bool
    -> unit
    -> ( [ `DirectoryNotFound | `DirectoryDeleted ]
         , [> Error.read_error | Error.delete_error ] )
         result

  (** Deletes a directory. If recursive is true (defaults to false), deletes all contents recursively.

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
    -> (unit, [> Error.delete_error ]) result

  (** Checks if a directory exists.

      Examples:

      {[
        match Dir.exists "mydir" with
        | Ok exists ->
          if exists then
            print_endline "Directory exists"
          else
            print_endline "Directory does not exist"
        | Error e -> print_endline (Dir.Error.to_string e)
      ]} *)
  val exists : string -> (bool, [> Error.read_error ]) result

  module Entry : sig
    (** Gets the name of a directory entry, whether it's a file or directory *)
    val get_name : entry -> string

    (** Returns true if the entry is a file *)
    val is_file : entry -> bool

    (** Returns true if the entry is a directory *)
    val is_directory : entry -> bool
  end
end
