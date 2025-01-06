# Fs

`Fs` aims to expose a simpler filesystem interface for OCaml.

## Installation

```sh
opam pin fs https://github.com/lessp/fs.git
```

## Example

For a full list of examples, see the [`examples/`](./examples) directory.

### File

#### Create a file

```ocaml
match Fs.File.write "example.txt" ~content:(String "Hello, World!") with
| Ok () -> print_endline "File written!"
| Error e -> print_endline (Fs.File.Error.to_string e)
```

#### Read a file

As a string:

```ocaml
match Fs.File.read_as_string "hello.txt" with
| Ok content -> print_endline content
| Error e -> print_endline (Fs.File.Error.to_string e)
```

In custom format:

```ocaml
match Fs.File.read "hello.txt" ~format:Bytes with
| Ok content -> print_endline (Bytes.to_string content)
| Error e -> print_endline (Fs.File.Error.to_string e)
```

### Dir

#### Create a directory

```ocaml
match Fs.Dir.create "hello" () with
| Ok () -> print_endline "Directory created!"
| Error e -> print_endline (Fs.Dir.Error.to_string e)
```

```ocaml
match Fs.Dir.create "hello/nested" ~recursive:true () with
| Ok () -> print_endline "Directories created!"
| Error e -> print_endline (Fs.Dir.Error.to_string e)
```

#### List a directory

```ocaml
match Fs.Dir.list "hello" with
| Ok files -> List.iter print_endline files
| Error e -> print_endline (Fs.Dir.Error.to_string e)
```

## License

MIT
