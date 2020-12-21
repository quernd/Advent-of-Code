#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my %hash;

while (my $line = <>) {
    $line =~ /^(\w+ \w+) bags contain (.*)$/;
    my $lhs = $1;
    my $rest = $2;
    while ($rest =~ /\d+ (\w+ \w+) bag/g) {
        my $rhs = $1;
        push @{$hash{$rhs}}, $lhs;
    }
    
}

my %done;
my %containing;
my @to_do = ('shiny gold');

while (@to_do) {
    my $color = shift @to_do;
    next if $done{$color};
    for my $ccolor (@{$hash{$color}}) {
        $containing{$ccolor} = 1;
        push @to_do, $ccolor;
    }
    $done{$color} = 1;
}

say scalar keys %containing;

