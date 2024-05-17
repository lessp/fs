module Error : sig
  type common_error =
    [ `File_not_found of string | `File_already_exists of string ]

  type read_error = [ `Error_reading_file of string ]
  type write_error = [ `Error_writing_to_file of string ]
  type t = [ read_error | write_error | common_error ]

  val to_string : t -> string
end

module File : sig
  type readonly
  type writeable
  type 'mode t

  val get_name : 'mode t -> string
  val get_content : 'mode t -> [ `String ] -> string

  val read :
    string -> (readonly t, [> Error.read_error | Error.common_error ]) result
  (** Reads the whole content of a file. After reading the file is closed. 

      Examples:

      {[
        match File.read "file.txt" with
        | Ok file -> print_endline (File.get_content file `String)
        | Error e -> print_endline (Error.to_string e)
      ]} 
  *)

  val read_as_string :
    string -> (string, [> Error.read_error | Error.common_error ]) result
  (** Reads the whole content of a file as a string. After reading the file is closed. 

      Examples:

      {[
        match File.read_as_string "file.txt" with
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
    contents:[ `String of string ] ->
    (unit, [> Error.write_error | Error.common_error ]) result
  (** Writes content to a file. After writing the file is closed. 

      Examples:

      {[
        match File.write "file.txt" ~contents:(`String "Hello, World!") with
        | Ok () -> print_endline "File written successfully"
        | Error e -> print_endline (Error.to_string e)
      ]}
  *)

  val open_file :
    string ->
    mode:[ `Readonly | `Writeable ] ->
    ('any t, [> Error.common_error ]) result
  (** Opens a file for reading. The file is closed when the reference is dropped. 

      Examples:

      {[
        match File.open_file "file.txt" None with
        | Ok file -> print_endline (File.get_content file `String)
        | Error e -> print_endline (Error.to_string e)
      ]}
  *)

  (* val create : string -> ('any t, 'err) result *)
end
