#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my %rules;
my %polymer;

my $start = <>;
chomp $start;

my $first = substr($start, 0, 1);

while ($start =~ /(?=(..))/g) {
    $polymer{$1}++;
}

while (<>) {
    chomp;
    if (/(.)(.) -> (.)/) {
        $rules{"$1$2"} = ["$1$3", "$3$2"];
    }
}

for my $step (1..40) {
    my %new_polymer;
    for my $lhs (keys %polymer) {
        my $number = $polymer{$lhs};
        for my $rhs (@{$rules{$lhs}}) {
            $new_polymer{$rhs} += $number;
        }
    }
    %polymer = %new_polymer;
    print_elements(%polymer) if ($step == 10 or $step == 40);
}

sub print_elements {
    my %polymer = @_;
    my %elements;
    for my $pair (keys %polymer) {
        $elements{substr($pair, 1, 1)} += $polymer{$pair};
    }
    $elements{$first}++;

    %elements = reverse %elements;
    my @elements = sort { $a <=> $b } (keys %elements);
    say($elements[$#elements] - $elements[0]);
}