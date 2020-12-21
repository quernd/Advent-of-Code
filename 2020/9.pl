#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my @numbers;

while (my $line = <>) {
    chomp $line;
    if (@numbers < 25) {
        push @numbers, $line;
    }
    else {
        my %hash = map { $_ => 1 } @numbers;
        my $found = 0;
        for my $number (@numbers) {
            if ($hash{$line - $number}) {
                $found = 1;
            }
        }
        if ($found == 1) {
            shift @numbers;
            push @numbers, $line;
        }
        else {
            say $line;
            exit;
        }
    }
}
