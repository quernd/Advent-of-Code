#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

my $sum = 0;

while (my $expression = <>) {
    chomp $expression;
    while ($expression !~ /^\d+$/) {
        if ($expression =~ s/\((\d+)\)/$1/
            or $expression =~ s/(\d+) ([+*]) (\d+)/$2 eq '+' ? $1 + $3 : $1 * $3/e) {
            # warn $expression;
        }
    }
    $sum += $expression;
}

say $sum;
