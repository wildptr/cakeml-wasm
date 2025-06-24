#!/usr/bin/perl
use v5.038;

print "[bits 64]\n";
print "section .data\n";
print "global cake_data\n";
print "cake_data:\n";

my $S = 0;
while (<>) {
    if ($S==0) {
        if (/^cake_bitmaps:$/) {
            $S=1;
        }
    }
    else {
        if (/^\s*\.quad (.*)/) {
            print "dq $1\n"
        }
        else {
            #print "cake_databuffer:\n";
            #print "cake_databuffer_end:\n";
            last
	}
    }
}
