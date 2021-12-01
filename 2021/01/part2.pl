#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;
use List::Util qw(sum);

my $count = 0;
my @prev = ();

while (<>) {
    my $prev = sum(@prev);
    push @prev, $_;

    if (@prev > 3) {
        shift @prev;
        $count++ if sum(@prev) > $prev
    }
}

say $count;