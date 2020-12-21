#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

use List::Util qw(min max sum);

my @numbers = sort { $a <=> $b } map({ chomp; $_} <>);

my $final = max(@numbers) + 3;

push @numbers, $final;


#print "@numbers";

my %hash = ( 0 => 1 );

for (@numbers) {
    $hash{$_} = ($hash{$_-1} // 0)
        + ($hash{$_-2} // 0)
        + ($hash{$_-3} // 0);
}

say $hash{$final};
