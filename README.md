# OCaml FS

FS exposes a simple filesystem interface for OCaml.

## Installation

```sh
opam pin fs https://github.com/lessp/fs.git
```

## Example

For a full list of examples, see the [`examples/`](./examples) directory.

```ocaml
open Fs

let () =
    match File.read "my-test.txt" with
    | Ok contents -> Printf.printf "File contents: %s\n" contents
    | Error e -> Printf.printf "Error reading file: %s" (Fs.Error.to_string e)
    ;;

let () =
    match File.write "my-test.txt" ~contents:(`String "Hello, world!") with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error writing file: %s" (Fs.Error.to_string e)
;;
```

## License

MIT
