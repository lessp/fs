module Error : sig
  type file_not_found = [ `File_not_found of string ]
  type file_already_exists = [ `File_already_exists of string ]
  type read_error = [ `Error_reading_file of string ]
  type write_error = [ `Error_writing_to_file of string ]
  type t = [ read_error | write_error | file_not_found | file_already_exists ]

  val to_string : t -> string
end

module File : sig
  type t

  val get_name : t -> string
  (** Returns the name of the file. 

      Examples:

      {[
        let file = File.create "file.txt" () |> Result.get_ok in
        print_endline (File.get_name file)
      ]}
  *)

  val get_content : t -> format:[ `String ] -> string
  (** Returns the contents of the file. 

      Examples:

      {[
        let file = File.create "file.txt" ~contents:(`String "Hello, World!") () |> Result.get_ok in
        print_endline (File.get_content file ~format:`String)
      ]}
  *)

  val read :
    string ->
    format:[ `String ] ->
    (t, [> Error.read_error | Error.file_not_found ]) result
  (** Reads the contents of a file. After reading, the file is closed. 

      Examples:

      {[
        match File.read "file.txt" ~format:`String with
        | Ok file -> print_endline (File.get_content file `String)
        | Error e -> print_endline (Error.to_string e)
      ]} 
  *)

  val read_to_string :
    string -> (string, [> Error.read_error | Error.file_not_found ]) result
  (** Reads the contents of a file as a string. After reading the file is closed. 

      Examples:

      {[
        match File.read_to_string "file.txt" with
        | Ok content -> print_endline content
        | Error e -> print_endline (Error.to_string e)
      ]}

      {[
        let contents = File.read_as_string "file.txt" |> Result.get_ok in
        print_endline contents
      ]}
  *)

  val write :
    string ->
    contents:
      [ `String of string
      | `Bytes of bytes
      | `Char of char
      | `Byte of int
      | `Substring of string * int * int
      | `Bigarray of
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        * int
        * int ] ->
    (unit, [> Error.write_error | Error.file_not_found ]) result
  (** Writes content to a file. After writing the file is closed. 

      Examples:

      {[
        match File.write "file.txt" ~contents:(`String "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (Error.to_string e)
      ]}
  *)

  val create :
    string ->
    ?contents:
      [ `String of string
      | `Bytes of bytes
      | `Char of char
      | `Byte of int
      | `Substring of string * int * int
      | `Bigarray of
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        * int
        * int ] ->
    ?overwrite:bool ->
    unit ->
    ( unit,
      [> Error.write_error | Error.file_not_found | Error.file_already_exists ]
    )
    result
  (** Creates a new file. 

      Examples:

      {[
        match File.create "file.txt" () with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (Error.to_string e)
      ]}

      {[
        match
          File.create "file.txt" ~contents:(`String "Hello, world!") ~overwrite:true ()
        with
        | Ok file -> print_endline (File.get_name file)
        | Error e -> print_endline (Error.to_string e)
      ]}
  *)

  val delete : string -> (unit, [> Error.file_not_found ]) result
  (** Deletes a file. 

      Examples:

      {[
        match File.delete "file.txt" with
        | Ok () -> print_endline "File deleted successfully"
        | Error e -> print_endline (Error.to_string e)
      ]}
  *)
end
