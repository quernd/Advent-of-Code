#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $sum = 0;

while (<>) {
    chomp;
    #say $_;
    if (/^(.*) \| (.*)$/) {
        my @inputs = split ' ', $1;
        my @outputs = split ' ', $2;
        $sum += grep {/^(..|...|....|.......)$/} @outputs;
    }
}

say $sum;