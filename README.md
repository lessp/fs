# OCaml FS

FS exposes a simple filesystem interface for OCaml.

## Example

For a full list of examples, see the `examples/` directory.

```ocaml
open Fs

match File.read "my-test.txt" with
| Ok contents -> Printf.printf "File contents: %s\n" contents
| Error e -> Printf.printf "Error reading file: %s" (Fs.Error.to_string e)
;;

match File.write "my-test.txt" ~contents:String "Hello, world!" with

```
