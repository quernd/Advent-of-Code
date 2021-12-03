#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my @oxygen;
my @scrubber;

while (<>) {
    chomp;
    my @digits = split(//, $_);
    push @oxygen, \@digits;
    push @scrubber, \@digits;
}

my $len = scalar @{$oxygen[0]};

for my $pos (0..$len-1) {
    if (@oxygen > 1) {
        my @ones = grep { $_->[$pos] == 1 } @oxygen;
        my @zeros  = grep { $_->[$pos] == 0 } @oxygen;
        if (@zeros > @ones) {
            @oxygen = @zeros;
        }
        else {
            @oxygen = @ones;
        }
    }
    if (@scrubber > 1) {
        my @ones = grep { $_->[$pos] == 1 } @scrubber;
        my @zeros  = grep { $_->[$pos] == 0 } @scrubber;
        if (@ones < @zeros) {
            @scrubber = @ones;
        }
        else {
            @scrubber = @zeros;
        }
    }
}

my $oxygen = join('', @{$oxygen[0]});
my $scrubber = join('', @{$scrubber[0]});

$oxygen = eval("0b$oxygen");
$scrubber = eval("0b$scrubber");

say($scrubber * $oxygen);