#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

sub diff {
    my ($a, $b) = @_;
    my @as = split '', $a;
    my @bs = split '', $b;
    return map { my $item = $_; (grep { $item eq $_ } @bs) ? () : ($item) } @as;
}

my $output_sum = 0;

while (<>) {
    chomp;
    if (/^(.*) \| (.*)$/) {

        my @inputs = split ' ', $1;
        my @outputs = split ' ', $2;

        my %all_digits = map {(join '', (sort (split '', $_))) => 1} (@inputs, @outputs);
        my @all_digits = keys %all_digits;
        
        my %digits;

        ($digits{1}) = (grep /^..$/, @all_digits);
        ($digits{7}) = (grep /^...$/, @all_digits);
        ($digits{4}) = (grep /^....$/, @all_digits);
        ($digits{8}) = (grep /^.......$/, @all_digits);

        for my $dig (grep /^.....$/, @all_digits) {
            if (diff($digits{1}, $dig) == 0) {
                $digits{3} = $dig;
            }
            elsif (diff($dig, $digits{4}) == 2) {
                $digits{5} = $dig;
            }
            else {
                $digits{2} = $dig;
            }
        }
        for my $dig (grep /^......$/, @all_digits) {
            if (diff($digits{4}, $dig) == 0) {
                $digits{9} = $dig;
            }
            elsif (diff($digits{1}, $dig) == 0) {
                $digits{0} = $dig;
            }
            else {
                $digits{6} = $dig;
            }
        }
        my %lookup = reverse %digits;

        my $output_value = join '', map {
            my $sorted = join '', (sort (split '', $_));
            $lookup{$sorted};
        } @outputs;
        
        $output_sum += $output_value;
    }
}

say $output_sum;