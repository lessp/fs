> [!NOTE]
> Exploratory, API subject to change. Suggestions and PRs are always welcome.

# Fs

`Fs` aims to expose a simple filesystem interface for OCaml.

## Table of Contents

- [Installation](#installation)
- [Example](#example)
  - [File](#file)
    - [Create a file](#create-a-file)
    - [Read a file](#read-a-file)
  - [Dir](#dir)
    - [Create a directory](#create-a-directory)
    - [List a directory](#list-a-directory)
- [License](#license)

## Installation

```sh
opam pin fs https://github.com/lessp/fs.git
```

## Example

For a full list of examples, see the [`examples/`](./examples) directory.

### File

#### Create and write to a file

```ocaml
match Fs.File.write "example.txt" ~content:(String "Hello, World!") with
| Ok () -> print_endline "File written!"
| Error e -> print_endline (Fs.File.Error.to_string e)
```

```ocaml
match Fs.File.append_string "example.txt" ~content:"Hello, World!" with
| Ok () -> print_endline "File written!"
| Error e -> print_endline (Fs.File.Error.to_string e)
```

#### Read a file

As a string:

```ocaml
match Fs.File.read_string "hello.txt" with
| Ok content -> print_endline content
| Error e -> print_endline (Fs.File.Error.to_string e)
```

Using a custom format:

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
match Fs.Dir.list "hello" () with
| Ok files -> List.iter print_endline files
| Error e -> print_endline (Fs.Dir.Error.to_string e)
```

## License

MIT
