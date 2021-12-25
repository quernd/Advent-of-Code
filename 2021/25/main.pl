#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use Data::Dumper;
$Data::Dumper::Indent = 0;

my @cucumbers;


while (<>) {
    chomp;
    push @cucumbers, [split '', $_];
}


my $width = scalar @{$cucumbers[0]};
my $height = scalar @cucumbers;

my $prev = "";

sub dump_state {
    for my $y (0..$height - 1) {
        for my $x (0..$width - 1) {
            print $_[$y]->[$x] // '.';
        }
        print "\n";
    }
    print "\n";
}

my $steps = 0;
while ((Dumper \@cucumbers) ne $prev) {
    $prev = Dumper \@cucumbers;
    #dump_state(@cucumbers);
    for my $herd ('>', 'v') {
        my @new_cucumbers;
        for my $x (0..$width - 1) {
            for my $y (0..$height - 1) {
                my $cucumber = $cucumbers[$y]->[$x];
                if (($cucumber // '.') eq $herd) {
                    my $x_ = $herd eq '>' ? ($x + 1) % $width : $x;
                    my $y_ = $herd eq 'v' ? ($y + 1) % $height : $y;
                    if (($cucumbers[$y_]->[$x_] // '.') eq '.') {
                        $new_cucumbers[$y_]->[$x_] = $cucumber;
                    }
                    else {
                        $new_cucumbers[$y]->[$x] = $cucumber;
                    }
                }
                elsif (($cucumber // '.') ne '.') {
                    $new_cucumbers[$y]->[$x] = $cucumber;
                }
            }
        }
        @cucumbers = @new_cucumbers;
    }
    $steps++;
}

say $steps;