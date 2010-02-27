/* AdventureController */

#import <Cocoa/Cocoa.h>

@interface AdventureController : NSObject
{
	IBOutlet NSTextView* textView;
	IBOutlet NSTokenField* tokenField;
	IBOutlet NSButton* enterButton;
	unsigned lastPos;
}

-(id) init;
-(IBAction) enterPressed:(id)sender;
// NSTokenField delegate methods
- (NSArray *)tokenField:(NSTokenField *)tokenField 
	completionsForSubstring:(NSString *)substring 
	indexOfToken:(int)tokenIndex 
	indexOfSelectedItem:(int *)selectedIndex;
- (NSString *)tokenField:(NSTokenField *)tokenField 
    editingStringForRepresentedObject:(id)representedObject;
- (id)tokenField:(NSTokenField *)tokenField 
    representedObjectForEditingString:(NSString *)editingString;

-(void) writeText :(NSString*)text;
-(void) prologEngine :(id)anObject;

// NSTextView delegate methods
-(void)keyUp:(NSEvent *)theEvent;
- (NSArray *)textView:(NSTextView *)textView completions:(NSArray *)words forPartialWordRange:(NSRange)charRange indexOfSelectedItem:(int *)index;
- (NSString *)textView:(NSTextView *)textView willDisplayToolTip:(NSString *)tooltip forCharacterAtIndex:(unsigned int)characterIndex;




@end
