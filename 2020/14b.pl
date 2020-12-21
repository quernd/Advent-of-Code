#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(sum);

my %mem;
my @mask;

while (my $line = <>) {
    if ($line =~ /mask = ([X01]*)/) {
        @mask = split //, $1;
    }
    elsif ($line =~ /mem\[(\d*)\] = (\d*)/) {
        my $address = $1;
        my $value = $2;

        my @addresses = (0);

        # Walk the address and decompose bit by bit
        for my $pos (0..35) {
            my $power = 2**(35 - $pos);
            my $bit = 0;

            # Bit is set if power of 2 still fits in address
            if ($address >= $power) {
                $address -= $power;
                $bit = 1;
            }

            # Apply the mask bit to the address
            # 0: keep address bit
            if ($mask[$pos] eq '0') {
                @addresses = map {$_ + $bit * $power} @addresses;
            }
            # 1: Bit always set 
            elsif ($mask[$pos] eq '1') {
                @addresses = map {$_ + $power} @addresses;
            }
            # X: floating bit -- create a copy with the bit set
            # (keep the existing addresses = "bit not set") 
            elsif ($mask[$pos] eq 'X') {
                push @addresses, (map {$_ + $power} @addresses);
            }
        }
        
        for my $address (@addresses) {
            $mem{$address} = $value;
        }
    }
}

say sum(map {$mem{$_}} (keys %mem));
