#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my @counts;

while (<>) {
    chomp;
    my @digits = split(//, $_);
    for my $pos (0..$#digits) {
        $counts[$pos]->{$digits[$pos]}++;
    }
}

my $gamma = "";   # most common digits
my $epsilon = ""; # least common digits

for my $pos (0..$#counts) {
    if ($counts[$pos]->{0} > $counts[$pos]->{1}) {
        $gamma .= 0;
        $epsilon .= 1;
    }
    else {
        $gamma .= 1;
        $epsilon .= 0;
    }
}

$gamma = eval("0b$gamma");
$epsilon = eval("0b$epsilon");

say($gamma * $epsilon);