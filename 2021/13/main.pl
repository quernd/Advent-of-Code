#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my %map;
my @folds;

while (<>) {
    chomp;
    if (/(\d+,\d+)/) {
        $map{$_} = 1;
    }
    elsif (/fold along ((x|y)=(\d+))/) {
        push @folds, $1;
    }
}

my $max_x = 1_000_000;
my $max_y = 1_000_000;

for my $fold (@folds) {
    $fold =~ /(x|y)=(\d+)/;
    my $dim = $1;
    my $axis = $2;

    $max_x = $axis if $dim eq 'x';
    $max_y = $axis if $dim eq 'y';

    for my $coord (keys %map) {
        if ($map{$coord} and $coord =~ /(\d+),(\d+)/) {
            if ($dim eq 'y' and $2 > $axis) {
                $map{$coord} = 0;
                my $new_y = $axis - ($2 - $axis);
                $map{"$1,$new_y"} = 1;
            }
            elsif ($dim eq 'x' and $1 > $axis) {
                $map{$coord} = 0;
                my $new_x = $axis - ($1 - $axis);
                $map{"$new_x,$2"} = 1;
            }
        }
    }
    say scalar (grep { $map{$_} == 1 } (keys %map));
}

for my $y (0..$max_y) {
    for my $x (0..$max_x) {
        print $map{"$x,$y"} // " ";
    }
    print "\n";
}