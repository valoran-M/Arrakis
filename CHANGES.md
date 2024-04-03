# 1.1.0 (???)

- Migration to codeberg
- Added a `--version` option to display current arrakis version
- Fixed a misalignment issue in shell help
- Fixed a misalignment issue in `--help`
- Changed message from `Error` to `Info` in case of failed system call
- Error message when providing too much input files
- Changed the `--no-shell` option to `--run` and `-ns` to `-r`
- Support for `.ascii` assembler directive
- Renamed commands to be closer to `gdb`:
  + `prv` is now `p`
  + `next` is now `continue`
  + `print` is now `info`
  + `run` is now `finish`
  + `reset` is now `run`
- Improved `info` command: Now always at most 80 character wide, more readable
  and easier to parse
- Greatly improved the codebase

# 1.0.0 (2023-19-9)

initial release

