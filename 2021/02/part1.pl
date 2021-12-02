#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my $horizontal = 0;
my $vertical = 0;

while (<>) {
    $horizontal += $1 if /forward (\d+)/;
    $vertical += $1 if /down (\d+)/;
    $vertical -= $1 if /up (\d+)/;
}

say ($horizontal * $vertical);

