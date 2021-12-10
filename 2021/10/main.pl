#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

my %penalty = (
    ')' => 3,
    ']' => 57,
    '}' => 1197,
    '>' => 25137,
);

my %other = (
    ')' => '(',
    ']' => '[',
    '}' => '{',
    '>' => '<',
);

my %points = (
    '(' => 1,
    '[' => 2,
    '{' => 3,
    '<' => 4,
);

my $corrupted = 0;
my @incomplete = ();

while (<>) {
    chomp;
    my @sequence = split '', $_;

    my @stack = ();
    for my $token (@sequence) {
        if ($token =~ /[([{<]/) {
            unshift @stack, $token;
        }
        elsif ($token =~ /[)\]}>]/) {
            if (shift @stack ne $other{$token}) {
                $corrupted += $penalty{$token};
                @stack = ();
                last;
            }
        }
    }
    if (@stack) {
        my $acc = 0;
        for my $token (@stack) {
            $acc = $acc * 5 + $points{$token};
        }
        push @incomplete, $acc;
    }
}

say $corrupted;

@incomplete = sort { $a <=> $b } @incomplete;
say $incomplete[$#incomplete / 2];