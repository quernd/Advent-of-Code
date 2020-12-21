#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

$/ = q();

my $unique = 0;

while (my $group = <>) {
    chomp $group;
    my @persons = split /\n/, $group;
    $group = join '', @persons;
    #warn $group;
    my @questions = split '', $group;
    my %unique;
    for my $q (@questions) {
        #warn $q;
        $unique{$q} += 1 if $q =~ /[a-z]/;
    }
    for (keys %unique) {
        $unique += 1 if $unique{$_} == @persons;
    }
}

say $unique;

