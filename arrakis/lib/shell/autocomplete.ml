let complete (input : string) (commands : string list) =
  List.filter (String.starts_with ~prefix:input) commands

