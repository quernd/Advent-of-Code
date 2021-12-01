#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $count = 0;
my $prev = undef;

while (<>) {
    $count++ if defined $prev and $_ > $prev;
    $prev = $_;
}

say $count;