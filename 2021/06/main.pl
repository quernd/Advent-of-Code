#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(sum);

my $lanternfish = <>;
chomp $lanternfish;

my @lanternfish = split ',', $lanternfish;

my @generations;

for my $fish (@lanternfish) {
    $generations[$fish]++;
}

sub grow_population {
    my $days = shift;
    my @generations = @_;
    for my $day (1..$days) {
        my @new_generations;
        for (0..7) {
            $new_generations[$_] = $generations[$_ + 1];
        }
        $new_generations[8] = $generations[0];
        $new_generations[6] += $generations[0] // 0;
        @generations = @new_generations;
    }
    return sum(@generations);
}

say grow_population(80, @generations);
say grow_population(256, @generations);