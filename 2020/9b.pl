#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

use List::Util qw(min max sum);

my @numbers;

while (my $line = <>) {
    chomp $line;
    push @numbers, $line;
}


for my $start (0..$#numbers) {
    for my $end ($start+1..$#numbers) {
        my @subsequence = @numbers[$start..$end];
        if (sum(@subsequence) == 3199139634) {
            say min(@subsequence) + max(@subsequence);
        }
    }
}
