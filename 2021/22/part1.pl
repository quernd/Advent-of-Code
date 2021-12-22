#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max sum);

my @instructions;

while (<>) {
    if (/(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/) {
        push @instructions, [$1, min($2, $3), max($2, $3), min($4, $5), max($4, $5), min($6, $7), max($6, $7)];
    }
}

my %on;

for my $instruction (@instructions) {
    my ($onoff, $x1, $x2, $y1, $y2, $z1, $z2) = @{$instruction};
    for ($x1, $y1, $z1) {
        $_ = max(-50, $_);
    }
    for ($x2, $y2, $z2) {
        $_ = min(50, $_);
    }
    for my $x ($x1..$x2) {
        for my $y ($y1..$y2) {
            for my $z ($z1..$z2) {
                if ($onoff eq 'on') {
                    $on{"$x,$y,$z"} = 1;
                }
                else {
                    $on{"$x,$y,$z"} = 0;
                }
            }
        }
    }
}

say(sum (map { $on{$_} } keys %on));