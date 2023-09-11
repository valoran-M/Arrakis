# Options

| Short Option | Long Option | Description                                     |
|--------------|-------------|-------------------------------------------------|
| h            | help        | Display the list of options, then exit          |
| U            |             | Use unix socket                                 |
| f            |             | Specify a Unix socket file                      |
|              | allow-root  | Allow usage of arrakis in root mode             |
|              | no-color    | Remove usage of color in output                 |
| e            |             | Set environment for ecalls. Either venus or unix|

## Some additionnal information

* Why isn't running Arrakis as root allowed by default?

  In short: Safety reason.
  Arrakis provide full access to [`ecall`](./ecall.md).
  Any `ecall` will be made with the permissions of the user running Arrakis.
  As such, running Arrakis as root allow almost any supported system call to be
  made.
  Since Arrakis was primarly made with education in mind, the user shouldn't be
  trusted to know what he is doing by default, as this could potentially be a
  security risk.

