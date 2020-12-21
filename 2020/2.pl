#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $counter = 0;

while (<>) {
    /^(\d+)-(\d+) (\w+): (\w+)$/;
    my $min = $1;
    my $max = $2;
    my $re = qr/$3/;
    my $password = $4;
    my $matches = () = $password =~ /$re/g;
    $counter += 1 if $matches >= $min and $matches <= $max;
}

say $counter;
