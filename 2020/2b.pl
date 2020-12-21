#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $counter = 0;

while (<>) {
    /^(\d+)-(\d+) (\w+): (\w+)$/;
    my $first = $1;
    my $second = $2;
    my $letter = $3;
    my $password = $4;
    $counter += 1 if substr($password, $first - 1, 1) eq $letter
        xor substr($password, $second - 1, 1) eq $letter;
}

say $counter;
