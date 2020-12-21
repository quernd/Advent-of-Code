#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(any all product);

my %cubes;

while (my $line = <>) {
    chomp $line;
    my @cubes = split //, $line;
    for (0..$#cubes) {
        $cubes{"$.:$_:0"} = $cubes[$_];
    }
}

my $current = \%cubes;

for (0..5) {
    #use Data::Dumper; $Data::Dumper::Sortkeys = 1; warn Dumper $current;
    my %neighbors;

    for my $coordinate (keys %$current) {
        my ($x, $y, $z) = split /:/, $coordinate;
        for my $x_ (-1, 0, +1) {
            for my $y_ (-1, 0, +1) {
                for my $z_ (-1, 0, +1) {
                    my $coordinate_ = sprintf "%s:%s:%s",
                        ($x + $x_), ($y + $y_), ($z + $z_);
                    next if $coordinate_ eq $coordinate;
                    $neighbors{$coordinate_} += 1 if $current->{$coordinate} eq '#';
                }
            }
        }
    }
    
    my %new_cubes;
    for (keys %neighbors) {
        if ($neighbors{$_} == 3) { # active no matter what
            $new_cubes{$_} = '#';
        }
        elsif ($neighbors{$_} == 2 and ($current->{$_} // '.') eq '#') {
            $new_cubes{$_} = '#';
        }
        else {
            $new_cubes{$_} = '.';
        }
    }

    $current = \%new_cubes;
    

}

say scalar (grep { $current->{$_} eq '#' } keys %$current);
