#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $PART2 = 1;

my %covered;

while (<>) {
    if (/^(\d+),(\d+) -> (\d+),(\d+)$/) {
        my ($x1, $y1, $x2, $y2) = ($1, $2, $3, $4);
        if ($x1 eq $x2) {
            ($y1, $y2) = ($y2, $y1) if $y2 < $y1;
            for my $y ($y1..$y2) {
                $covered{"($x1,$y)"} += 1;
            }
        }
        elsif ($y1 eq $y2) {
            ($x1, $x2) = ($x2, $x1) if $x2 < $x1;
            for my $x ($x1..$x2) {
                $covered{"($x,$y1)"} += 1;
            }
        }
        elsif ($PART2 and abs($x2 - $x1) == abs($y2 - $y1)) {
            my $xsign = $x2 > $x1? 1 : -1;
            my $ysign = $y2 > $y1? 1 : -1;

            my $length = abs($x2 - $x1);
            for my $delta (0..$length) {
                my $x = $x1 + ($delta * $xsign);
                my $y = $y1 + ($delta * $ysign);
                $covered{"($x,$y)"} += 1;
            }
        }
    }
    else {
        die "Parsing error.";
    }
}

say (scalar (grep {$covered{$_} > 1} (keys %covered)));
