#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max);

my @square;
while (<>) {
    if (/^Player (\d+) starting position: (\d+)$/) {
        $square[$1 - 1] = $2;
    }
}

my $winning = 1000;

my $round = 0;
my $rolled = 0;
my @score = (0, 0);

while (max(@score) < $winning) {
    my $player = $round % 2;
    for (0..2) {
        $square[$player] += $rolled % 100 + 1;
        $rolled++;
    }
    $square[$player] = (($square[$player] - 1) % 10) + 1;
    $score[$player] += $square[$player];
    $round++;
}

say min(@score) * $rolled;