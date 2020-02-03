# Clogger

A minor mode for logging emacs commands to a buffer. When enabled in a buffer, any commands executed in that buffer are logged to the `*commands*` buffer.

## Usage

Clone to a folder `/path/to/folder/clogger`.

Then add the following to your .emacs file:
```lisp
(add-to-list 'load-path "/path/to/folder/clogger/")
(require 'clogger)
```

Then enable in a buffer with `M-x clogger-mode`.
