#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $input = <>;
chomp $input;

my ($x1, $x2, $y1, $y2);
# target area: x=20..30, y=-10..-5
if ($input =~ /target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/) {
    $x1 = $1;
    $x2 = $2;
    $y1 = $3;
    $y2 = $4;
}
else {
    die "Bad input.";
}

# After a few steps, y will be 0, so the max initial velocity
# is the maximum that will not overshoot in the next step
my $y_v = -$y1 - 1;
# nod to Gauss
say($y_v * ($y_v + 1) / 2);

sub simulate {
    my ($x_v, $y_v) = (@_);
    my ($x, $y) = (0, 0);
    while ($y >= $y1) {
        return 1 if ($x >= $x1 and $x <= $x2 and $y >= $y1 and $y <= $y2);
        $x += $x_v;
        $y += $y_v;
        $x_v-- if $x_v > 0;
        $x_v++ if $x_v < 0;
        $y_v--;
    }
    return 0;
}

my $min_y = $y1;
my $max_y = -$y1 - 1;
my $max_x = $x2;

my $count = 0;
for my $i (0..$max_x) {
    for my $ii ($min_y..$max_y) {
        $count += simulate($i, $ii);
    }
}

say $count;