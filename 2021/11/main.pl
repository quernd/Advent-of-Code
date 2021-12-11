#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my %map;

my $y = 0;
while (<>) {
    chomp;
    my $x = 0;
    for (split '', $_) {
        $map{"$x $y"} = $_;
        $x++;
    }
    $y++;
}

sub neighbors {
    my ($x, $y) = split ' ', shift;
    my @neighbors;
    for ([-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]) {
        my ($x_, $y_) = @$_;
        my $n_x = $x + $x_;
        my $n_y = $y + $y_;
        push @neighbors, "$n_x $n_y";
    }
    return @neighbors;
}

sub print_map {
    for my $x (0..9) {
        for my $y (0..9) {

        }
    }
}

my @flashes;
my $flashes = 0;

my $step = 0;

while (1) {
    $step++;
    for (keys %map) {
        $map{$_}++;
    }
    #use Data::Dumper; warn Dumper \%map;
    my %has_flashed = ();
    my $still_flashing = 1;
    while ($still_flashing) {
        $still_flashing = 0;

        for (keys %map) {
            if ($map{$_} > 9) {
                for my $neighbor (neighbors($_)) {
                    if (defined ($map{$neighbor})) {
                        $map{$neighbor}++ unless $has_flashed{$neighbor};
                    }
                }
                $still_flashing = 1;
                $flashes++;
                $map{$_} = 0;
                $has_flashed{$_} = 1;
            }
        }
    }
    push @flashes, $flashes;
    last if (keys %has_flashed == keys %map);
}

say $step;
say $flashes[99];
