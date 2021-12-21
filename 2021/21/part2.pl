#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(max);

my $winning = 21;

my @square;
while (<>) {
    if (/^Player (\d+) starting position: (\d+)$/) {
        $square[$1 - 1] = $2;
    }
}

my %universes = (0 => {"0,0,$square[0],$square[1],0" => 1});
my $p1_wins;
my $p2_wins;

for (0..100) {
    my $universes = $universes{$_};
    SCORE: for my $score (keys %{$universes}) {
        my $universe = $universes->{$score};
        my ($s1, $s2, $sq1, $sq2, $p) = split ',', $score;
        if ($s1 >= $winning) {
            $p1_wins += $universe;
            next SCORE;
        }
        if ($s2 >= $winning) {
            $p2_wins += $universe;
            next SCORE;
        }
        my $next_p = 1 - $p;
        for my $d1 (1..3) {
            for my $d2 (1..3) {
                for my $d3 (1..3) {
                    my $d = $d1 + $d2 + $d3;
                    my $new_sq1 = $p == 0 ? ($sq1 + $d - 1) % 10 + 1 : $sq1;
                    my $new_sq2 = $p == 1 ? ($sq2 + $d - 1) % 10 + 1 : $sq2;
                    my $new_s1 = $p == 0 ? $s1 + $new_sq1 : $s1;
                    my $new_s2 = $p == 1 ? $s2 + $new_sq2 : $s2;
                    my $new_score = "$new_s1,$new_s2,$new_sq1,$new_sq2,$next_p";
                    my $total = $new_s1 + $new_s2;
                    $universes{$total}->{$new_score} += $universe;
                }
            }
        }
    }
}

say max($p1_wins, $p2_wins);