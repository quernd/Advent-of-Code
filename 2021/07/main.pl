#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(sum min max);

my $crabs = <>;
chomp $crabs;

my @crabs = split ',', $crabs;

my @fuel_part1;
my @fuel_part2;

for my $position (0..max @crabs) {
    push @fuel_part1, sum(map { abs($_ - $position) } @crabs);
    push @fuel_part2, sum(map { my $n = abs($_ - $position); ($n * ($n + 1)) / 2 } @crabs);
}

say(min(@fuel_part1));
say(min(@fuel_part2));