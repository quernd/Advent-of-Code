#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my @instructions;

while (my $line = <>) {
    $line =~ /^(acc|jmp|nop) ([+-]\d*)$/;
    push @instructions, { instruction => $1,
                          value => $2,
    };
}




for (0..$#instructions) {
    # flip instruction
    if ($instructions[$_]->{instruction} eq 'nop') {
        $instructions[$_]->{instruction} = 'jmp';
    }
    elsif ($instructions[$_]->{instruction} eq 'jmp') {
        $instructions[$_]->{instruction} = 'nop';
    }
    my $a = terminate();
    if (defined $a) {
        say $a;
        exit;
    }
    if ($instructions[$_]->{instruction} eq 'nop') {
        $instructions[$_]->{instruction} = 'jmp';
    }
    elsif ($instructions[$_]->{instruction} eq 'jmp') {
        $instructions[$_]->{instruction} = 'nop';
    }    
}



sub terminate {
    my $acc = 0;
    my $index = 0;
    my %visited;

    for (;;) {
        return $acc if $index == @instructions;
        my $i = $instructions[$index];
        return undef if $visited{$i};
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
        $visited{$i} = 1;
    }
}
