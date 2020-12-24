#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use Data::Dumper;

my %tiles;

my $days = shift @ARGV // 0;

while (my $tile = <>) {
    chomp $tile;
    next unless $tile;

    # We can simulate a hexagonal grid in a square grid
    # by going *two* steps in the east-west axis
    $tile =~ s/(?<![ns])([ew])/$1$1/g;
    my $north = () = $tile =~ /n/g;
    my $south = () = $tile =~ /s/g;
    my $east = () = $tile =~ /e/g;
    my $west = () = $tile =~ /w/g;

    my $y = $north - $south;
    my $x = $east - $west;

    $tiles{"$y:$x"} = 1 - ($tiles{"$y:$x"} // 0);
}

my $black_tiles = () = grep {$tiles{$_} == 1} keys %tiles;
say "Day 0: ", $black_tiles;


for my $day (1..$days) {
    my %neighbors;
    my @black_tiles = grep {$tiles{$_} == 1} keys %tiles;
    for my $coordinate (@black_tiles) {
        # To make sure this tile is included in the flipping process!
        $neighbors{$coordinate} += 0;
        
        my ($y, $x) = split ':', $coordinate;

        for my $x_ (1, -1) {
            my $neighbor1 = sprintf "%s:%s", $y - 1, $x + $x_;
            my $neighbor2 = sprintf "%s:%s", $y + 1, $x + $x_;
            my $neighbor3 = sprintf "%s:%s", $y, $x + 2 * $x_;
            for my $neighbor ($neighbor1, $neighbor2, $neighbor3) {
                $neighbors{$neighbor} += 1;
            }
        }
    }

    for my $coordinate (keys %neighbors) {
        my $color = $tiles{$coordinate} // 0;
        my $neighbors = $neighbors{$coordinate};
        if ($color == 1) {
            if ($neighbors == 0 or
                $neighbors > 2) {
                $tiles{$coordinate} = 0;
            }
        }
        elsif ($color == 0) {
            if ($neighbors == 2) {
                $tiles{$coordinate} = 1;
            }
        }
    }
    my $black_tiles_after_day = () = grep {$tiles{$_} == 1} keys %tiles;
    say "Day $day: ", $black_tiles_after_day; 
}
