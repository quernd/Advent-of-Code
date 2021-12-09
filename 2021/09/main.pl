#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max sum product all);

my @map;

while (<>) {
    chomp;
    push @map, [split '', $_];
}

my $height = scalar @{$map[0]};
my $width = scalar @map;

my @basins;

for my $x (0..$width - 1) {
    for my $y (0..$height - 1) {
        my @neighbors;
        for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
            my ($x_delta, $y_delta) = @$dir;
            my $x_ = max(0, min($width - 1, $x + $x_delta));
            my $y_ = max(0, min($height - 1, $y + $y_delta));
            push @neighbors, [$x_, $y_] if $x_ != $x or $y_ != $y;
        }
        if (all { my ($x_, $y_) = @{$_}; 
                  $map[$x_]->[$y_] > $map[$x]->[$y] } @neighbors) {
            push @basins, [$x, $y];
        }
    } 
}

say sum(map { my ($x, $y) = @{$_}; $map[$x]->[$y] + 1 } @basins);


sub grow_basin {
    my ($x, $y) = @{$_[0]};
    my @frontier = ([$x, $y]);
    my %basin = ("$x $y" => 1);
    while (@frontier) {
        my @new_frontier;
        for my $point (@frontier) {
            my ($x, $y) = @{$point};
            my @neighbors;
            for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
                my ($x_delta, $y_delta) = @{$dir};
                my $x_ = max(0, min($width - 1, $x + $x_delta));
                my $y_ = max(0, min($height - 1, $y + $y_delta));
                push @neighbors, [$x_, $y_] if $x_ != $x or $y_ != $y;
            }
            for my $neighbor (@neighbors) {
                my ($x_, $y_) = @{$neighbor};
                if ($map[$x_]->[$y_] > $map[$x]->[$y]
                    and $map[$x_]->[$y_] != 9) {
                    unless ($basin{"$x_ $y_"}) {
                        $basin{"$x_ $y_"} = 1;
                        push @new_frontier, [$x_, $y_];
                    }
                }
            }
        }
        @frontier = @new_frontier;
    }
    return keys %basin;
}

my @basin_sizes = sort { $b <=> $a } (map { scalar grow_basin($_) } @basins);

say product(@basin_sizes[0..2]);