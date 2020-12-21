#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my @instructions;

while (my $line = <>) {
    $line =~ /^(acc|jmp|nop) ([+-]\d*)$/;
    push @instructions, { instruction => $1,
                          value => $2,
                          visited => 0,
    };
}

my $acc = 0;
my $index = 0;

while ($instructions[$index]->{visited} == 0) {
    my $i = $instructions[$index];
    if ($i->{instruction} eq 'acc') {
        $acc += $i->{value};
        $index += 1;
    }
    elsif ($i->{instruction} eq 'jmp') {
        $index += $i->{value};
    }
    else {
        $index += 1;
    }
    $i->{visited} = 1;
}

say $acc;

