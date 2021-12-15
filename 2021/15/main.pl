#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min);

my @map;

while (<>) {
    chomp;
    my @line = split '', $_;
    push @map, \@line;
}

my $height = @map;
my $width = @{$map[0]};

sub offset {
    my ($num, $max) = @_;
    my $offset = 0;
    while ($num >= $max) {
        $num -= $max;
        $offset++;
    }
    return ($num, $offset);
}

sub get_risk {
    my ($x, $y, $times) = @_;
    if ($x < 0 or $y < 0 or $x >= ($width * $times) or $y >= ($height * $times)) {
        return undef;
    }
    my ($x_, $x_offset) = offset($x, $width);
    my ($y_, $y_offset) = offset($y, $height);

    my $risk = $map[$y_]->[$x_] + $x_offset + $y_offset;
    return $risk == 9 ? 9 : $risk % 9;
}

sub find_path {
    my $times = shift;
    my %risk = ("0-0" => 0);
    my %final = ();
    while (keys %risk) {
        my @sorted = sort { $risk{$a} <=> $risk{$b} } keys(%risk);
        my $next = $sorted[0];
        my $risk = $risk{$next};
        $final{$next} = $risk;
        delete $risk{$next};
        my ($x, $y) = split '-', $next;
        my @neighbors = ([$x + 1, $y], [$x - 1, $y], [$x, $y + 1], [$x, $y - 1]);
        for my $neighbor (@neighbors) {
            my ($n_x, $n_y) = @{$neighbor};
            unless ($final{"$n_x-$n_y"}) {
                my $neighbor_risk = get_risk($n_x, $n_y, $times);
                if (defined $neighbor_risk) {
                    my $old_risk = $risk{"$n_x-$n_y"};
                    my $new_risk = min(grep { defined } ($old_risk, $neighbor_risk + $risk));
                    $risk{"$n_x-$n_y"} = $new_risk;
                }
            }
        }
    }

    return $final{($height * $times - 1) . "-" . ($width * $times - 1)};
}

say find_path(1);
say find_path(5);
