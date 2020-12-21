#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

use List::Util qw(min max sum);

my @numbers = sort { $a <=> $b } map({ chomp; $_} <>);
push @numbers, max(@numbers) + 3;

#print "@numbers";

my $jolt = 0;
my %hash;

for (@numbers) {
    my $diff =  $_ - $jolt;
    $jolt = $_;
    $hash{$diff} += 1;
}

say $hash{1} * $hash{3};
