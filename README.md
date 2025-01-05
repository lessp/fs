# Fs

`Fs` aims to expose a simpler filesystem interface for OCaml.

## Installation

```sh
opam pin fs https://github.com/lessp/fs.git
```

## Example

For a full list of examples, see the [`examples/`](./examples) directory.

```ocaml
open Fs

let () =
    match File.write "my-test.txt" ~contents:(`String "Hello, world!") with
    | Ok () -> Printf.printf "File written successfully\n"
    | Error e -> Printf.printf "Error writing file: %s" (Fs.Error.to_string e)

let () =
    match File.read_to_string "my-test.txt" with
    | Ok contents -> Printf.printf "File contents: %s\n" contents
    | Error e -> Printf.printf "Error reading file: %s" (Fs.Error.to_string e)
    ;;
;;
```

## Todo

- [ ] Add the ability to read a file into different formats
- [ ] Add directory operations
- [ ] Consider using `open_gen` to be able to specify flags for file operations

## License

MIT
