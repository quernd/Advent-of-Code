#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

$/ = q();

my $unique = 0;

while (my $group = <>) {
    chomp $group;
    $group =~ s/\n//g;
    #warn $group;
    my @questions = split '', $group;
    my %unique;
    for my $q (@questions) {
        #warn $q;
        $unique{$q} = 1 if $q =~ /[a-z]/;
    }
    $unique += scalar keys %unique;
}

say $unique;

