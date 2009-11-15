//
//  main.m
//  adventure
//
//  Created by Adrian Prantl on 08.11.09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import <Cocoa/Cocoa.h>

int the_argc;
char **the_argv;

int main(int argc, char *argv[])
{
	the_argc=argc;
	the_argv=argv;
    return NSApplicationMain(argc,  (const char **) argv);
}
