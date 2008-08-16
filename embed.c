int
main(int argc, char **argv)
{ 
#ifdef READLINE /* Remove if you don't want readline */
  PL_initialise_hook(install_readline);
#endif

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  PL_halt(PL_toplevel() ? 0 : 1);
}
