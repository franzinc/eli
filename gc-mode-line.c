/* gc-mode-line.c	-[Thu Aug  1 15:31:34 1996 by layer]-
 *
 * This and the accompanying gc-mode-line.cl file implement `Run Bars' in the
 * Emacs mode line for Common Lisp buffers, both interaction and
 * source buffers.
 * See the comments in that file for instructions.
 * $Id: gc-mode-line.c,v 2.2 1996/08/01 22:36:46 layer Exp $
 */

static int mode_line_fd = 0;
static char *mode_line;
static char *mode_line_save;
extern int (*gc_before)(),(*gc_after)();

int mode_line_gc()
{
  if (mode_line_fd) {
    mode_line_save = mode_line;
    run_bar_hook(0,"GC  ");
  }
}

int mode_line_gc_end()
{
  if (mode_line_fd)
    run_bar_hook(0,mode_line_save);
}

run_bar_hook(op, arg)
     int op, arg;
{
  switch (op) {
  case 0:			/* set mode line */
    mode_line = (char *) arg;
    return (write(mode_line_fd,mode_line, 4));
    break;
   case 1:			/* Init run bars */
    mode_line_fd = arg;
     if (arg) {
       gc_before = mode_line_gc;
       gc_after = mode_line_gc_end;
     } else {
       gc_before = 0;
       gc_after = 0;
     }
    break;
  case 2:			/* Run */
    if (mode_line_fd)
      run_bar_hook(0,"Run ");
    break;
  case 3:			/* Idle */
    if (mode_line_fd)
      run_bar_hook(0,"Idle");
    break;
  }
  return(0);
}
