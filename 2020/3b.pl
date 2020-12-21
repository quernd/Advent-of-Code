#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my @lines;

while (my $line = <>) {
    chomp $line;
    my @line = split q(), $line;
    push @lines, \@line;
}

my @counters;

for ([1, 1], [3, 1], [5, 1], [7, 1], [1, 2]) {
    my ($right, $down) = @$_;
    my $counter = 0;
    my $offset_r = 0;

    for my $i (0..$#lines) {
        if ($i % $down == 0) {
            $offset_r = $offset_r % @{$lines[$i]};
            $counter += 1 if $lines[$i]->[$offset_r] =~ /\#/;
            $offset_r += $right;
        }
    }
    push @counters, $counter;
}

my $result = 1;
for (@counters) {
    $result *= $_;
}

say $result;
