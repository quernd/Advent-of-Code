#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

my $sum = 0;

while (my $expression = <>) {
    chomp $expression;

    # Inserting lots of parentheses to hack operator precedence
    # https://en.wikipedia.org/wiki/Operator-precedence_parser#Alternative_methods
    # Although not obvious, the algorithm was correct, and, in the words of Knuth, “The resulting formula is properly parenthesized, believe it or not.”
    $expression =~ s/\(/(((/g;
    $expression =~ s/(^|(?<=\())/((/;
    $expression =~ s/\)/)))/g;
    $expression =~ s/$/))/;
    $expression =~ s/\*/))*((/g; # multiplication lower precedence
    $expression =~ s/\+/)+(/g;   # addition higher precedence
    $expression =~ s/ *//g;      # delete all whitespace

    $sum += eval($expression);
    next;

    # Alternative:
    
    # Reduce the expression down to a single number
    while ($expression !~ /^\d+$/) {
        if ($expression =~ s/\((\d+)\)/$1/  # (number) => number
            # Addition and multiplication: /e computes result
            # before substitution
            or $expression =~ s/(\d+)\+(\d+)/$1 + $2/e
            or $expression =~ s/(\d+)\*(\d+)/$1 * $2/e) {
            # warn $expression; # for debugging
        }
        else {
            die "Can't reduce expression: $expression";
        }
    }
    $sum += $expression;
}

say $sum;
