#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max);

sub window {
    my $coord = shift;
    my ($x, $y) = split ',', $coord;
    my @window;
    for my $i (-1..1) {
        for my $ii (-1..1) {
            push @window, (($x + $ii) . ',' . ($y + $i));
        }
    }
    return @window;
}

my $enhancement = <>;
my @enhancement = split '', $enhancement;

my %start_map = ("inf" => ".");
my $y = 0;
while (my $line = <>) {
    chomp $line;
    my @line = split '', $line;
    for my $x (0..$#line) {
        $start_map{"$x,$y"} = $line[$x];
    }
    $y++;
}

sub bounds {
    my @xs;
    my @ys;
    for my $coord (keys %{$_[0]}) {
        next if $coord eq "inf";
        my ($x, $y) = split ',', $coord;
        push @xs, $x;
        push @ys, $y;
    }
    return (min(@xs) - 1, max(@xs) + 1, min(@ys) - 1, max(@ys) + 1);
}

sub enhance {
    my %map = %{$_[0]};
    my $placeholder = $_[1];
    my %new_map;
    my ($min_x, $max_x, $min_y, $max_y) = bounds(\%map);
    for my $x ($min_x..$max_x) {
        for my $y ($min_y..$max_y) {
            my $coord = "$x,$y";
            my @window = window($coord);
            my @string;
            for my $coord_ (@window) {
                push @string, ($map{$coord_} // $map{"inf"}) eq '#' ? 1 : 0;
            }
            my $string = join '', @string;
            my $index = eval("0b$string");
            $new_map{$coord} = $enhancement[$index];
        }
    }
    $new_map{"inf"} = $enhancement[eval("0b" . (($map{"inf"} eq '#' ? 1 : 0) x 9))];
    return \%new_map;
}

for (1..50) {
    %start_map = %{enhance(\%start_map, $_ % 2 ? '.' : '#')};
    if ($_ == 2 or $_ == 50) {
        say scalar (grep { $start_map{$_} eq '#' } (keys %start_map));
    }
}

sub dump_map {
    my %map = %{$_[0]};
    my ($min_x, $max_x, $min_y, $max_y) = bounds(\%map);
    for my $y ($min_y..$max_y) {
        for my $x ($min_x..$max_x) {
            my $coord = "$x,$y";
            print $map{$coord} // '.';
        }
        print "\n";
    }
}