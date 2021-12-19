#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(max sum);

sub manhattan_distance {
    my ($scanner1, $scanner2) = @_;
    my $distance = sum(map { abs($scanner1->[$_] - $scanner2->[$_]) } (0..2));
}

sub rotate {
    my ($beacon, $rotation) = @_;
    my ($x, $y, $z) = @{$beacon};
    my ($rx, $ry, $rz) = @{$rotation};
    ($y, $z) = (-$z, $y) for (1..$rx);
    ($z, $x) = (-$x, $z) for (1..$ry);
    ($x, $y) = (-$y, $x) for (1..$rz);
    return [$x, $y, $z];
}

sub translate {
    my ($beacon, $translation) = @_;
    return [map { $beacon->[$_] - $translation->[$_] } (0..2)];
}

sub scanner_overlap {
    my ($scanner1, $scanner2) = @_;
    my %translations;
    for my $beacon1 (@{$scanner1}) {
        for my $beacon2 (@{$scanner2}) {
            my $x_offset = $beacon2->[0] - $beacon1->[0];
            my $y_offset = $beacon2->[1] - $beacon1->[1];
            my $z_offset = $beacon2->[2] - $beacon1->[2];            
            $translations{"$x_offset,$y_offset,$z_offset"}++;
        }
    }
    for (keys %translations) {
        return [split ',', $_] if ($translations{$_} >= 12);
    }
    return undef;
}

# This is just to filter out half of the orientations as they are redundant
my @orientations;
my %orientations;
for my $rx (0..3) {
    for my $ry (0..3) {
        for my $rz (0..3) {
            my ($x, $y, $z) = @{rotate([1, 2, 3], [$rx, $ry, $rz])};
            my $rotated = "$x,$y,$y";
            push @orientations, [$rx, $ry, $rz] unless $orientations{$rotated};
            $orientations{$rotated} = 1;
        }
    }
}

my @scanners;
my $scanner;

while (<>) {
    chomp;
    if (/scanner (\d+)/) {
        $scanner = $1;
    }
    elsif (/(-?\d+),(-?\d+),(-?\d+)/) {
        push @{$scanners[$scanner]}, [$1, $2, $3];
    }
}

my %matched = (
    0 => [$scanners[0], [0, 0, 0]],
);

my @queue = (0,);

while (@queue) {
    my $next = shift @queue;
    SCANNER:
    for my $i (0..$#scanners) {
        unless ($i == $next or $matched{$i}) {
            for my $orientation (@orientations) {
                my $rotated_scanner = [map { rotate($_, $orientation) } @{$scanners[$i]}];
                my $translation = scanner_overlap($matched{$next}->[0], $rotated_scanner);
                if (defined $translation) {
                    my $translated_scanner = 
                        [map { translate($_, $translation) } @{$rotated_scanner}];
                    $matched{$i} = [$translated_scanner, $translation];
                    push @queue, $i;
                    next SCANNER;
                }
            }            
        }
    }
}

my %beacons;
for my $scanner (keys %matched) {
    for my $beacon (@{$matched{$scanner}->[0]}) {
        $beacons{"$beacon->[0],$beacon->[1],$beacon->[2]"}++;
    }
}

my @distances;
for my $i (0..$#scanners) {
    for my $ii (0..$#scanners) {
        push @distances, manhattan_distance($matched{$i}->[1], $matched{$ii}->[1]);
    }
}

say scalar keys %beacons;
say max @distances;