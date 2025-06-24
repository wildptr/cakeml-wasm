#!/usr/bin/perl
use v5.038;

my $S=0;
while (<>) {
    if ($S) {
        if (/^#/) {exit}
        print;
    }
    else {
        if (/^# after filter_skip/) {$S=1}
    }
}
