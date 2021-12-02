#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $horizontal = 0;
my $vertical = 0;

my $aim = 0;

while (<>) {
    if (/forward (\d+)/) {
        $horizontal += $1;
        $vertical += $aim * $1;
    };
    $aim += $1 if /down (\d+)/;
    $aim -= $1 if /up (\d+)/;
}

say ($horizontal * $vertical);
