#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(sum);

my @last_seen;
my $input = "0,12,6,13,20,1"; #,17";

my @numbers = split /,/, $input;
    
for (0..$#numbers) {
    $last_seen[$numbers[$_]] = $_;
}

my $number = 17;
   
for(my $index = 6; $index < $ARGV[0] - 1; $index++) {
    #last if $index >= $ARGV[0] - 1;
    my $new_number = defined $last_seen[$number] ?
        $index - $last_seen[$number] : 0;

    $last_seen[$number] = $index;
    $number = $new_number;
}

say $number;
