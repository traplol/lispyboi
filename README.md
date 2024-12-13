# Lispyboi

Lispyboi is a bytecode-based Lisp implementation written in C++. It features a virtual machine with garbage collection, tail-call optimization, and a moderate standard library.

## Features

### Core Language
- Lisp-2 dialect (separate namespace for functions and variables)
- Lexical scoping
- First-class functions and closures
- Macro system with quasiquotation
- Garbage collection with generational collection
- Tail-call optimization
- Package system
- Rich type system
- CLOS-like generic functions

### Standard Library
- Comprehensive sequence operations (lists, arrays, strings)
- File I/O with streams
- String manipulation with UTF-8 support
- Network programming (TCP sockets, SSL support)
- Foreign Function Interface (FFI)
- Testing framework
- Error handling system

### Example Applications
- HTTP server with routing and file serving
- Echo server/client network application

## Building

### Prerequisites
- Modern C++ compiler (C++11 or later)
- OpenSSL development libraries
- Make

### Build Steps
```bash
# Build the main executable
make

# Run tests
./lispyboi lisp/test-suite.module
```

## Usage

### Running the REPL
```bash
./lispyboi
```

### Running a Script
```bash
./lispyboi script.lisp
```

### HTTP Server Example
```bash
./lispyboi lisp/httpd.lisp 8080
```

## Language Basics

### Core Forms
```lisp
;; Function definition
(defun greet (name)
  (format t "Hello, ~a!~%" name))

;; Macro definition
(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

;; Object system
(defstruct point
  (x 0)
  (y 0))

;; Generic functions
(defgeneric area (shape)
  "Calculate area of a shape")

(defmethod area ((s circle))
  (* pi (square (circle-radius s))))
```

### Package System
```lisp
(defpackage :my-package
  (:use :lispyboi)
  (:export :my-function))

(in-package :my-package)
```

### FFI Example
```lisp
(require "ffi")

(ffi-with-symbols "libc.so.6"
  ((c-printf "printf"))
  (c-printf "Hello from C!\n"))
```

## Architecture

### Virtual Machine
- Stack-based bytecode VM
- Two-stack design (parameter stack and return stack)
- Three execution modes:
  1. Computed gotos (fastest)
  2. Tailcall optimization
  3. Switch-based dispatch (debug mode)

### Memory Management
- Generational garbage collector
- Mark-and-sweep collection
- Value tagging for efficient representation

### Type System
- Tagged 64-bit values
- First 3 bits used for type tags
- Support for:
  - Fixnums (63-bit integers)
  - Cons cells
  - Symbols
  - Strings (UTF-8)
  - Arrays
  - Functions/closures
  - Characters
  - Packages
  - Generic functions

## License

This project is open source and available under the MIT License.

## Acknowledgments

This implementation takes inspiration from various Lisp dialects and implementations, particularly:
- Common Lisp
- Scheme
- SBCL's compilation model
- Emacs Lisp's simplicity
