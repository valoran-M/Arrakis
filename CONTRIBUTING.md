# Contributing

Arrakis is open to contributors!

The simplest way to help us is to simply use Arrakis and to give us feedback.
If you find any bug, please [open an issue](https://codeberg.org/Arrakis/arrakis/issues).
Be sure to include Arrakis version in your issue (available using `arrakis --version`).

You can also contribute to [Arrakis Wiki](https://codeberg.org/Arrakis/arrakis/wiki).

Issues and pull requests are also welcome on the [github mirror](https://github.com/valoran-M/Arrakis),
but please consider doing them on the [main repository](https://codeberg.org/arrakis/arrakis)
if possible.

## Contributing code

Please keep in mind that Arrakis is distributed under the [`CeCILL 2.1 license`](./LICENSE),
and that any code submitted will be redistributed under this same license.

You must own full right to any code submitted.
All commit must be [signed with a GPG key](https://docs.codeberg.org/security/gpg-key/).

Submitted code must also respect the following coding style:
* Wrap line at 80 characters (When reasonable to do so)
* Use two spaces instead of tabulation
* Remove any trailing white space

Stuff that are currently planned can be looked at in [`TODO.md`](./TODO.md).

## Making a release

Please be sure to update Arrakis version in [main.ml](./arrakis/bin/main.ml).
It should end with suffix '-dev' while being developed but the suffix should be
dropped before any release are made.

Be sure to run `dune build` so that `arrakis.opam` is up to date.

