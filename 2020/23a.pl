#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use Data::Dumper;

use List::Util qw(min max);

my $cups = $ARGV[0];

my @cups = split //, $cups;
my $highest = max(@cups);
my $lowest = min(@cups);

my $current = 0;

for my $round (1..$ARGV[1]) {
    #say "@cups";
    my $current_label = shift @cups;
    my $next0 = shift @cups;
    my $next1 = shift @cups;
    my $next2 = shift @cups;

    my $destination = $current_label;
    while ($destination == $next0 or
           $destination == $next1 or
           $destination == $next2 or
           $destination == $current_label) {
        $destination -= 1;
        $destination = $highest if $destination < $lowest;
    }

    # find destination
    for (0..$#cups) {
        if ($cups[$_] == $destination) {
            splice @cups, $_ + 1, 0, $next0, $next1, $next2;
            push @cups, $current_label;
            last;
        }
    }
    #say "@cups";
}

say "@cups";
