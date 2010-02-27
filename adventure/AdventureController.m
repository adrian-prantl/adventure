#import "AdventureController.h"
#include <SWI-Prolog.h>
#include <unistd.h>
#include <stdio.h>

// Yeah, side-effects!
// The communication between the SWI-Prolog thread and the Cocoa app is 
// controlled by a few global Variables, and an unnamed pipe.
extern int the_argc;
extern char **the_argv;
static id theController = nil;
static struct { int r; int w; } thePipe;
static NSMutableArray* completions = nil;
static NSLock* completionsLock = nil;
static bool completionsFull = NO;

#define enterChar(c) do { \
		unichar _c = c; \
		NSAssert(write(thePipe.w, &_c, sizeof(unichar)) == sizeof(unichar), @"I/O Error"); \
	} while (0)
 
void abort_interpreter() {
  PL_cleanup(0);
 // ??
}

foreign_t pl_getch(term_t a0)
{
  unichar c;
  term_t t1 = PL_new_term_ref();
  NSLog(@"PL waiting...");
  read(thePipe.r, &c, sizeof(unichar));
  NSLog(@"PL ok.");
  PL_put_integer(t1, c);
  PL_unify(a0, t1);
  PL_succeed;
}

foreign_t pl_write_xy(term_t a0, term_t a1, term_t a2)
{
  char *s;
  int x, y;//, old_x, old_y, new_x, new_y;
  if (PL_get_atom_chars(a0, &s) && 
      PL_get_integer(a1, &x) && 
      PL_get_integer(a2, &y)) {
	NSString* str = [NSString stringWithUTF8String :s];
	NSLog(str);
	  // FIXME  design a better interface than this...
    if (x == -1) {
    // if x, y < 0 use default cursor else draw at x,y and restore cursor pos
    //getyx(win, old_y, old_x);
    //move(y>=0?y:old_y, x>=0?x:old_x);
    //printw("%s", s);
    //getyx(win, new_y, new_x);
    //move(y>=0?old_y:new_y, x>=0?old_x:new_x)
		[theController writeText :str];
	} else if (x == 40) {
		NSLog(@"PL Writing '%s'...",s);
		[completionsLock lock];
		if (completionsFull) {
			[completions removeAllObjects];
			completionsFull = NO;
		}
		if (s[0] == '$') completionsFull = YES;
		else [completions addObject :str];
		[completionsLock unlock];
	} else {
	// Ignore
  }
    PL_succeed;
  } else {
    NSLog(@"ERROR!");
    exit(1);
    PL_fail;
  }
}

foreign_t pl_bold()   { 
	//[[theController textView] setFont :[NSFont boldSystemFontOfSize :14]];
	PL_succeed; 
}

foreign_t pl_italic() { 
	//[[theController textView] setFont :[NSFont systemFontOfSize:14]];
	PL_succeed; 
}

foreign_t pl_roman()  { 
	//[[theController textView] setFont :[NSFont menuFontOfSize :14]];
	PL_succeed;
}

@implementation AdventureController

- (id)init
{
	theController = [super init];
	NSAssert(pipe(&thePipe.r) == 0, @"Could not create pipe!");
	completions = [NSMutableArray new];
	completionsLock = [NSLock new];
	lastPos = 0;
	[NSThread detachNewThreadSelector:@selector(prologEngine:) toTarget:self withObject:nil];
	
	return theController;
}


-(void) writeText :(NSString*)text
{
	while (textView == nil) {
		sleep(1); // FIXME this is a horrible way to wait until the initialization of the UI is finished
	}
	[textView insertText:text];
}

- (IBAction)enterPressed:(id)sender
{
	enterChar('\n');
	lastPos=0; // FIXME (possible race condition? those two operations should be atomic)
    //[textView setString :[[tokenField objectValue] componentsJoinedByString :@" "]];
	[tokenField setObjectValue :nil];
}

- (NSArray *)tokenField:(NSTokenField *)tokenField 
	completionsForSubstring:(NSString *)substring 
	indexOfToken:(int)tokenIndex 
	indexOfSelectedItem:(int *)selectedIndex
{
	unsigned l = [substring length];
	while (lastPos > l) { // remove characters
		NSLog(@"User erased one character.");
		enterChar('\b');
		--lastPos;
	}

	while (lastPos < l) { // push characters
		unichar c = [substring characterAtIndex:lastPos];
		NSLog(@"User typed character '%c'.", c);
		enterChar(c);
		++lastPos;
	}
	NSAssert(lastPos == l, nil);
	
	[completionsLock lock];
	NSArray* copy = [NSArray arrayWithArray :completions];
	[completionsLock unlock];
	return copy;
}

- (NSString *)tokenField:(NSTokenField *)tokenField 
    editingStringForRepresentedObject:(id)representedObject
{
	return representedObject;
}

- (id)tokenField:(NSTokenField *)tokenField 
	representedObjectForEditingString:(NSString *)editingString
{
	NSLog(@"representedObjectForEditingString(%@)",editingString);
	// Remove newline
	return [editingString substringToIndex :[editingString length]-1];
}


// NSTextView delegate methods
- (void)keyUp:(NSEvent *)theEvent
{
	NSLog(@"YEAH! http://stackoverflow.com/questions/11291/cocoa-best-way-to-capture-key-events-in-nstextview");
}

- (NSArray *)textView:(NSTextView *)textView completions:(NSArray *)words 
	forPartialWordRange:(NSRange)charRange indexOfSelectedItem:(int *)index
{
	return [NSArray arrayWithObject: @"HOWDY!"];
}

- (NSString *)textView:(NSTextView *)textView willDisplayToolTip:(NSString *)tooltip forCharacterAtIndex:(unsigned int)characterIndex
{	
	return @"Hi there!";
}



-(void) prologEngine :(id)anObject
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  char *av[10];
  int ac = 0;

  av[ac++] = the_argv[0];
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
  
  // Register foreign predicates with the interpreter
  PL_register_foreign("getch", 1, pl_getch, 0);
  PL_register_foreign("write_xy", 3, pl_write_xy, 0);
  PL_register_foreign("bold", 0, pl_bold, 0);
  PL_register_foreign("italic", 0, pl_italic, 0);
  PL_register_foreign("roman", 0, pl_roman, 0);

  // Load our own modules
  term_t t1 = PL_new_term_ref();
  const char* path = [[NSString stringWithFormat:@"'%@/Contents/Resources/%@'", 
	[[NSBundle mainBundle] bundlePath], @"advcore2"] UTF8String];
  PL_chars_to_term(path, t1);
  PL_call_predicate(NULL, PL_Q_NORMAL, PL_predicate("consult", 1, ""), t1);

  // Run the module
  term_t goal = PL_new_term_ref();
  PL_chars_to_term("main", goal);
  PL_call(goal, PL_new_module(PL_new_atom("advcore2")));
  
  PL_halt(/*PL_toplevel() ?*/ 0/* : 1 */);
  [pool release];
  NSAssert(false, @"Prolog process died!");
}

@end
