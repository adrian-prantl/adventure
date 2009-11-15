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

-(void) sayText :(NSString*) text;
-(void) prologEngine :(id)anObject;

@end
