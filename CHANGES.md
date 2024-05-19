# 1.1.0 (???)

- Migration to codeberg
- Greatly improved the codebase
- Added a `--version` option to display current arrakis version
- Renamed the `--no-shell` option to `--run` and `-ns` to `-r`
- Fixed a misalignment issue in shell help
- Fixed a misalignment issue in `--help` option
- Changed message from `Error` to `Info` in case of a failed system call
- Renamed commands to be closer to `gdb`:
  + `prv` is now `p`
  + `next` is now `continue`
  + `print` is now `info`
  + `run` is now `finish`
  + `reset` is now `run`
- New command `breakpoint clear` to remove all breakpoints
- Improved `info` command: Now always at most 80 characters wide, more readable
  and easier to parse
- Error message when providing too much input files
- Support for `.ascii/z` assembler directive
- Support for program arguments
- Remove simple memory values
- Support for local labels
- Add multi line comments
- Support for GNU expressions
- Add `.size` directive for `info` command
- Add `next` command (Like `step` but don't step into functions)

# 1.0.0 (2023-19-9)

initial release

