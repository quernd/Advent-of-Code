#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $max = 0;

my %seats;

while (my $line = <>) {
    chomp $line;
    $line =~ /([FB]{7})([LR]{3})/;
    my $row = $1;
    my $col = $2;
    $row =~ y/FB/01/;
    $col =~ y/LR/01/;
    $row = eval("0b$row");
    $col = eval("0b$col");
    
    my $id = 8 * $row + $col;
    $max = $id if $id > $max;

    $seats{$id} = 1;
}

say $max;

for (0..$max) {
    say $_ if !defined $seats{$_} and $seats{$_+1} and $seats{$_-1};
}
