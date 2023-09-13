# Using Arrakis Shell

## Basic usage

| Long           | Short | Description                |
|----------------|-------|----------------------------|
| run            | r     | Run the code to the end    |
| step           | s     | Run a single instruction   |

## Breakpoints

| Long           | Short | Description                |
|----------------|-------|----------------------------|
| breakpoint     | p     | Add a new breakpoint       |
| next           | n     | Run to the next breakpoint |

## Printing information

Printing information about the system is done throught the `print` command.
This command take as first argument the part of the system to print, and some
more argument depending on the command.
Using the `print` command without any arguments will simply display the
documentation.

| Long           | Short | Description  |
|----------------|-------|--------------|
| print memory   | p m   |              |
| print register | p r   |              |
| print code     | p c   |              |
