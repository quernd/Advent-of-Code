#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max any);
use List::PriorityQueue;

use Data::Dumper;
$Data::Dumper::Indent = 0;

my %costs = (
    'A' => 1,
    'B' => 10,
    'C' => 100,
    'D' => 1000,
);

sub room {
    my $pos = shift;
    return 1 if any { $_ == $pos } (2, 4, 6, 8);
    return 0;
}


my %home = (
    'A' => 2,
    'B' => 4,
    'C' => 6,
    'D' => 8,
);

sub find_path {
    my ($depth, $from, $to) = @_;

    sub distance {
        my ($from, $to, @configuration) = @_;
        my ($left, $right) = (min($from, $to), max($from, $to));
        for my $pos ($left..$right) {
            unless (room($pos) or $pos == $from) {
                return undef if defined $configuration[$pos];
            }
        }
        return $right - $left;
    }

    sub room_to_room {
        my ($depth, $room1, $room2, @configuration) = @_;
        my $cost = distance($room1, $room2, @configuration);
        return undef unless defined $cost;
        $configuration[$room1] = [@{$configuration[$room1]}];
        my $amphipod = shift @{$configuration[$room1]};
        return undef unless defined $amphipod;
        return undef unless $home{$amphipod} == $room2;
        return undef if @{$configuration[$room2]} >= $depth;
        return undef if any { $amphipod ne $_ } (@{$configuration[$room2]});
 
        $cost += $depth - @{$configuration[$room1]};
        $cost += $depth - @{$configuration[$room2]};
        $configuration[$room2] = [@{$configuration[$room2]}];
        unshift @{$configuration[$room2]}, $amphipod;
        $cost *= $costs{$amphipod};
        return [$cost, \@configuration];
    }

    sub room_to_hallway {
        my ($depth, $room, $hallway, @configuration) = @_;
        my $cost = distance($room, $hallway, @configuration);
        return undef unless defined $cost;
        $configuration[$room] = [@{$configuration[$room]}];
        return undef unless any { $home{$_} != $room } @{$configuration[$room]};
        my $amphipod = shift @{$configuration[$room]};
        return undef unless defined $amphipod;
        return undef if defined $configuration[$hallway];

        $cost += $depth - @{$configuration[$room]};
        $cost *= $costs{$amphipod};
        $configuration[$hallway] = $amphipod;
        return [$cost, \@configuration];
    }

    sub hallway_to_room {
        my ($depth, $hallway, $room, @configuration) = @_;
        return undef unless defined $configuration[$hallway];
        my $amphipod = $configuration[$hallway];
        return undef unless $home{$amphipod} == $room;
        my $cost = distance($hallway, $room, @configuration);
        return unless defined $cost;
        $configuration[$room] = [@{$configuration[$room]}];
        return undef if any { $amphipod ne $_ } @{$configuration[$room]};
        return undef if @{$configuration[$room]} >= $depth;

        $cost += $depth - @{$configuration[$room]};
        unshift @{$configuration[$room]}, $amphipod;
        $configuration[$hallway] = undef;
        $cost *= $costs{$amphipod};
        return [$cost, \@configuration];
    }

    sub steps {
        my ($depth, @configuration) = @_;
        my @admissible;
        for my $room (2, 4, 6, 8) {
            for my $hallway (0, 1, 3, 5, 7, 9, 10) {
                push @admissible, room_to_hallway($depth, $room, $hallway, @configuration);
                push @admissible, hallway_to_room($depth, $hallway, $room, @configuration);
            }
        }
        for my $room1 (2, 4, 6, 8) {
            for my $room2 (2, 4, 6, 8) {
                push @admissible, room_to_room($depth, $room1, $room2, @configuration);
            }
        }
        return (grep { defined } @admissible);
    }

    my @queue = ([[$from, undef]]);
    my %final;

    for my $energy (0..100000) {
        my $next = $queue[$energy];
        for (@{$next}) {
            my ($configuration, $parent) = @{$_};
            my $key = Dumper $configuration;
            next if defined $final{$key};
            $final{$key} = [$energy, $parent];
            for (steps($depth, @{$configuration})) {
                my $cost = $_->[0];
                my $new_config = $_->[1];
                my $new_key = Dumper $new_config;
                my $new_cost = $cost + $energy;
                push @{$queue[$new_cost]}, [$new_config, $configuration] unless defined $final{$new_key};
            }
        }
    }

    return $final{Dumper $to}->[0];
}

my $depth = 2;
say find_path($depth, [undef,
    undef, ['D', 'C'], undef, ['D', 'C'], undef, 
    ['A', 'B'], undef, ['A', 'B'], undef, undef,],
    [undef, undef, [('A') x $depth], undef, [('B') x $depth], undef, [('C') x $depth], undef, [('D') x $depth], undef, undef]);
    
$depth = 4;
say find_path($depth, [undef,
    undef, ['D', 'D', 'D', 'C'], undef, ['D', 'C', 'B', 'C'], undef, 
    ['A', 'B', 'A', 'B'], undef, ['A', 'A', 'C', 'B'], undef, undef,],
    [undef, undef, [('A') x $depth], undef, [('B') x $depth], undef, [('C') x $depth], undef, [('D') x $depth], undef, undef]);
