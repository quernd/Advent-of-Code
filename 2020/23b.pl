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

while (@cups < 1_000_000) {
   $highest += 1;
   push @cups, $highest;
}

my @circular_list;
my $last;

for (@cups) {
    my $cup = {number => $_};
    $circular_list[$_] = $cup;
    $last->{succ} = $cup if defined $last;
    $last = $cup;
}

$last->{succ} = $circular_list[$cups[0]];

my $current_cup = $circular_list[$cups[0]];

for my $round (1..$ARGV[1]) {
    # find current cup

    my $next1 = $current_cup->{succ};
    my $next2 = $next1->{succ};
    my $next3 = $next2->{succ};

    #warn $next1->{number}, $next2->{number}, $next3->{number};

    # cut picked cups
    $current_cup->{succ} = $next3->{succ};
    my $current_label = $current_cup->{number};
    my $destination = $current_label;

    while ($destination == $current_label
           or $destination == $next1->{number}
           or $destination == $next2->{number}
           or $destination == $next3->{number}) {
        $destination--;
        $destination = $highest if $destination < $lowest;
    }

    # find destination and rewire the three picked cups
    my $destination_cup = $circular_list[$destination];
    my $succ = $destination_cup->{succ};
    $destination_cup->{succ} = $next1;
    $next3->{succ} = $succ;

    $current_cup = $current_cup->{succ};
}

my $one = $circular_list[1];
say $one->{succ}->{number} * $one->{succ}->{succ}->{number};
