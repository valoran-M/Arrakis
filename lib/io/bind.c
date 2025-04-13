/* Arrakis ********************************************************************/
/* Copyright 2023-2025 Arrakis contributors                                   */
/* Distributed under the CeCILL 2.1 LICENSE <http://www.cecill.info>          */
/******************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <stdbool.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>

/*
 * Inspired by Down :
 * https://github.com/dbuenzli/down/blob/master/src/down_stubs.c
 */

static struct termios restore = {0};
static bool activate = false;

value caml_init_shell(value unit)
{
    CAMLparam1 (unit);
    struct termios set;

    if (!activate) {
        if (!isatty(0))
            CAMLreturn(Val_false);
        if (tcgetattr(0, &restore) < 0)
            CAMLreturn(Val_false);

        set = restore;
        set.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        set.c_cflag |= (CS8);
        set.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
        set.c_cc[VMIN] = 1; set.c_cc[VTIME] = 0;

        if (tcsetattr(0, TCSAFLUSH, &set) < 0)
            CAMLreturn(Val_false);
        activate = true; ;
    }
    CAMLreturn(Val_true);
}

value caml_exit_shell(value unit)
{
    CAMLparam1(unit);
    if (activate) {
        if (tcsetattr(0, TCSAFLUSH, &restore) < 0)
            CAMLreturn(Val_false);
        activate = false;
    }
    CAMLreturn(Val_true);
}

value caml_readc(value unit)
{
    CAMLparam1 (unit);
    unsigned char buf;
    int ret;
    ret = read(0, &buf, 1);
    if (ret == 1) CAMLreturn(Val_int(buf));
    if (ret == 0) CAMLreturn(Val_int(-1));
    if (ret == -1 && errno == EINTR)
        CAMLreturn(Val_int(-2));
    CAMLreturn(Val_int(-3));
}

