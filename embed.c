#include <string.h>
#include <SWI-Prolog.h>
#include <curses.h>
#include <stdlib.h>

void abort_interpreter() {
  PL_cleanup(0);
  // Curses
  move(24, 1);
  clear();
  printw("Godbye!\n");
  endwin();
}

foreign_t pl_getch(term_t a0)
{
  term_t t1 = PL_new_term_ref();
  PL_put_integer(t1, getch());
  PL_unify(a0, t1);
  PL_succeed;
}

WINDOW* win;

foreign_t pl_write_xy(term_t a0, term_t a1, term_t a2)
{
  char *s;
  int x, y, old_x, old_y, new_x, new_y;
  if (PL_get_atom_chars(a0, &s) && 
      PL_get_integer(a1, &x) && 
      PL_get_integer(a2, &y)) {
    // if x, y < 0 use default cursor else draw at x,y and restore cursor pos
    getyx(win, old_y, old_x);
    move(y>=0?y:old_y, x>=0?x:old_x);
    printw("%s", s);
    getyx(win, new_y, new_x);
    move(y>=0?old_y:new_y, x>=0?old_x:new_x);
    PL_succeed;
  } else {
    printw("ERROR!");
    exit(1);
    PL_fail;
  }
}

foreign_t pl_bold()   { attron(A_BOLD); PL_succeed; }
foreign_t pl_italic() { attron(A_UNDERLINE); PL_succeed; }
foreign_t pl_roman()  { attrset(A_NORMAL); PL_succeed; }

int
main(int argc, char **argv)
{ 
/* #ifdef READLINE /\* Remove if you don't want readline *\/ */
/*   PL_initialise_hook(install_readline); */
/* #endif */
  char *av[10];
  int ac = 0;

  av[ac++] = argv[0];
  av[ac++] = strdup("-q");
  //av[ac++] = strdup("-O");
  // Sizes of "0" mean the largest possible limits.
  /* av[ac++] = strdup("-L0");  // Local stack */
  /* av[ac++] = strdup("-G0");  // Global stack */
  /* av[ac++] = strdup("-A0");  // Argument stack */
  /* av[ac++] = strdup("-T0");  // Trail stack */
  av[ac++] = strdup("-nosignals");
  atexit(abort_interpreter);
  av[ac]   = NULL;
  
  if ( !PL_initialise(ac, av) )
    PL_halt(1);
  
  // Curses Initialization
  win = initscr();
  timeout(-1);
  curs_set(1/*visibility*/);
  move(0,0);

  // Register foreign predicates with the interpreter
  PL_register_foreign("getch", 1, pl_getch, 0);
  PL_register_foreign("write_xy", 3, pl_write_xy, 0);
  PL_register_foreign("bold", 0, pl_bold, 0);
  PL_register_foreign("italic", 0, pl_italic, 0);
  PL_register_foreign("roman", 0, pl_roman, 0);

  // Load our own modules
  term_t t1 = PL_new_term_ref();
  PL_chars_to_term("advcore2", t1);
  PL_call_predicate(NULL, PL_Q_NORMAL, PL_predicate("consult", 1, ""), t1);

  // Run the module
  term_t goal = PL_new_term_ref();
  PL_chars_to_term("main", goal);
  PL_call(goal, PL_new_module(PL_new_atom("advcore2")));
  
  PL_halt(/*PL_toplevel() ?*/ 0/* : 1 */);
}
