#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $current = 90;
my %x = (0 => 0, 90 => 1, 180 => 0, 270 => -1);
my %y = (0 => 1, 90 => 0, 180 => -1, 270 => 0);
my $x = 0;
my $y = 0;

while (my $line = <>) {
    chomp $line;
    $line =~ /([NSEWLRF])(\d+)/;
    my $direction;
    if ($1 eq 'N') {
        $direction = 0;
    }
    elsif ($1 eq 'E') {
        $direction = 90;
    }
    elsif ($1 eq 'S') {
        $direction = 180;
    }
    elsif ($1 eq 'W') {
        $direction = 270;
    }
    elsif ($1 eq 'L') {
        $current -= $2;
        $current = $current % 360;
        next;
    }
    elsif ($1 eq 'R') {
        $current += $2;
        $current = $current % 360;
        next;
    }
    elsif ($1 eq 'F') {
        $direction = $current;
    }

    $x += $2 * $x{$direction % 360};
    $y += $2 * $y{$direction % 360};
}

say (abs($x) + abs($y));

