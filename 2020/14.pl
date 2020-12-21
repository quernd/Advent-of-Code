#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(sum);

my %mem;
my @mask;

while (my $line = <>) {
    if ($line =~ /mask = ([X01]*)/) {
        @mask = split //, $1;
    }
    elsif ($line =~ /mem\[(\d*)\] = (\d*)/) {
        my $acc = 0;
        my $location = $1;
        my $value = $2;
        for my $pos (0..35) {
            my $power = 2**(35 - $pos);
            my $bit = 0;
            if ($value >= $power) {
                $value -= $power;
                $bit = 1;
            }
            if ($mask[$pos] eq 'X') {
                $acc += $power * $bit;
            }
            else {
                $acc += $power * $mask[$pos];
            }
        }
        $mem{$location} = $acc;
    }
}

#use Data::Dumper;
#warn Dumper %mem;

say sum(map {$mem{$_}} (keys %mem));
