#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(product);

my $timestamp = <>; chomp $timestamp;
my $schedule  = <>; chomp $schedule;

my @buses = split /,/, $schedule;
my $buses = grep {$_ ne 'x'} @buses;

my %synced;

for ($timestamp = 0;;) {

    for (0..$#buses) {
        my $bus = $buses[$_];
        next if $bus eq 'x';
        $synced{$bus} = 1 if ($timestamp + $_) % $bus == 0;
    }
    last if keys %synced == $buses;

    $timestamp += product(keys %synced);
}

say $timestamp;
