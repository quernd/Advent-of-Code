#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my $timestamp = <>;
chomp $timestamp;

my $schedule = <>;
chomp $schedule;

my @buses = split /,/, $schedule;

my $shortest = "inf";
my $best = undef;

for my $bus (@buses) {
    next if $bus eq 'x';
    my $wait = $bus - ($timestamp % $bus);
    if ($wait < $shortest) {
        $shortest = $wait;
        $best = $bus;
    }
}

say $shortest * $best;
