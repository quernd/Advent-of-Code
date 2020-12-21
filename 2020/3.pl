#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $counter = 0;

my $offset = 0;

while (my $line = <>) {
    chomp $line;
    my @line = split q(), $line;
    $offset = $offset % @line;
    $counter += 1 if $line[$offset] =~ /\#/;
    $offset += 3;
}

say $counter;
