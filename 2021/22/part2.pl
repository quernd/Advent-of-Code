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

sub volume {
    my ($x1, $x2, $y1, $y2, $z1, $z2) = @_;
    return ($x2 - $x1 + 1) * ($y2 - $y1 + 1) * ($z2 - $z1 + 1);
}

sub cuboid_intersection {
    my $cube1 = shift;
    my $cube2 = shift;
    my $x1 = max($cube1->[0], $cube2->[0]);
    my $x2 = min($cube1->[1], $cube2->[1]);
    return undef if $x2 < $x1;
    my $y1 = max($cube1->[2], $cube2->[2]);
    my $y2 = min($cube1->[3], $cube2->[3]);
    return undef if $y2 < $y1;
    my $z1 = max($cube1->[4], $cube2->[4]);
    my $z2 = min($cube1->[5], $cube2->[5]);
    return undef if $z2 < $z1;
    return [$x1, $x2, $y1, $y2, $z1, $z2];
}

my @volumes;

for my $instruction (@instructions) {
    my ($on_off, @cuboid) = @{$instruction};
    for (my @volumes_ = @volumes) {
        my ($volume, $coords) = @{$_};
        if (my $intersection = cuboid_intersection($coords, \@cuboid)) {
            my $intersection_volume = volume(@{$intersection});
            push @volumes, [($volume > 0 ? -1 : 1) * $intersection_volume, $intersection];
        }
    }
    push @volumes, [volume(@cuboid), \@cuboid] if $on_off eq "on";
}

say sum (map { $_->[0] } @volumes);
