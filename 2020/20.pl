#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

use List::Util qw(product);

local $/ = '';


my @tiles;
my %tiles;

while (my $tile = <>) {
    my @tile = split /\n/, $tile;
    my $description = shift @tile;
    $description =~ /Tile (\d+):/;
    my $number = $1;
    my ($signature, $flipped_signature) = signature(\@tile);

    for ([0, 1, 2, 3], [1, 2, 3, 0], [2, 3, 0, 1], [3, 0, 1, 2]) {
        push @tiles, { number => $number,
                       top    => $signature->[$_->[0]],
                       right  => $signature->[$_->[1]],
                       bottom => $signature->[$_->[2]],
                       left   => $signature->[$_->[3]],
                       flipped => 0,
                       orientation => $_->[0],
        };
        push @tiles, { number => $number,
                       top    => $flipped_signature->[$_->[0]],
                       right  => $flipped_signature->[$_->[1]],
                       bottom => $flipped_signature->[$_->[2]],
                       left   => $flipped_signature->[$_->[3]],
                       flipped => 1,
                       orientation => $_->[0],
        };                 
    }

    $tiles{$number} = \@tile;
}

my %top;
my %left;

for (@tiles) {
    push @{$top{$_->{top}}}, $_;
    push @{$left{$_->{left}}}, $_;
}


my $solution = expand_board({}, [], 0, 0);
my @corners = ($solution->[0]->[0],   $solution->[0]->[-1],
               $solution->[-1]->[0], $solution->[-1]->[-1]);

say product(map {$_->{number} } @corners);


my @all_lines;

for my $y (0..11) {
    my @lines;
    for my $x (0..11) {
        my $tile = $solution->[$y]->[$x];
        my $pattern = $tiles{$tile->{number}}; 
        my $flipped = $tile->{flipped} ? flip($pattern) : $pattern;
        my $rotated = rotate($flipped, $tile->{orientation});
        my @cropped = @{crop($rotated)};
        for (0..$#cropped) {
            $lines[$_] .= $cropped[$_];
        }
    }
    push @all_lines, @lines;
}

my $monster = qr/
(?=[.#]{18}
\#
[.#]{1}
[.#_]{77}
\#
[.#]{4}
\#\#
[.#]{4}
\#\#
[.#]{4}
\#{3}
[.#_]{77}
[.#]
\#
[.#]{2}
\#
[.#]{2}
\#
[.#]{2}
\#
[.#]{2}
\#
[.#]{2}
\#
[.#]{3})
/x;

for my $flip (0, 1) {
    my @test_flip = @all_lines;
    my $test_flip = $flip ? flip(\@test_flip) : \@test_flip;

    for my $orientation (0, 1, 2, 3) {
        my @test_orientation = @$test_flip;
        
        my $grid = rotate(\@test_orientation, $orientation);
        my $string = join '_', @$grid;

        if (my $monsters = () = $string =~ /$monster/g) {
            say "Flip $flip, orientation $orientation -- $monsters monsters";
            my $roughness = () = $string =~ /\#/g;
            say ($roughness - 15 * $monsters);
        }
    }
}
    



                  # 
#    ##    ##    ###
 #  #  #  #  #  #   

#say join "\n", @all_lines;

sub expand_board {
    # used tiles, constraints, next square
    my $used_tiles = shift;
    my $placed_tiles = shift;
    my $x = shift;
    my $y = shift;
    #warn "$x $y";
    return $placed_tiles if $y == 12;
    #use Data::Dumper; warn Dumper $placed_tiles;
    my @available_tiles = grep { !$used_tiles->{$_->{number}} } @tiles;
    if ($x > 0) {
        my $left_neighbor = $placed_tiles->[$y]->[$x - 1];
        my $constraint = reverse $left_neighbor->{right};
        @available_tiles = grep { $_->{left} eq $constraint } @available_tiles;
    }
    if ($y > 0) {
        my $top_neighbor = $placed_tiles->[$y - 1]->[$x];
        my $constraint = reverse $top_neighbor->{bottom};
        @available_tiles = grep { $_->{top} eq $constraint } @available_tiles;
    }

    
    # place a tile on next available square
    for my $tile (@available_tiles) {

        $used_tiles->{$tile->{number}} = 1;
        $placed_tiles->[$y]->[$x] = $tile;
        # call recursively

        my $next_x = $x + 1;
        my $next_y = $y;

        if ($next_x == 12) {
            $next_y += 1;
            $next_x = 0;
        }

        if (my $ret = expand_board($used_tiles, $placed_tiles, $next_x, $next_y)) {
            return $ret;
        }
        else {
            # retract placed tile
            delete $used_tiles->{$tile->{number}};
            $placed_tiles->[$y]->[$x] = undef;
        }
    }

    # No tiles could be placed.
    return;
                             
}


sub signature {
    my $tile = shift;
    my $top = $tile->[0];
    my $bottom = $tile->[-1];

    my $left = join '', map { (split '', $_)[0] } @$tile;
    my $right = join '', map { (split '', $_)[-1] } @$tile;

    my @signature = ($top, $right, (scalar reverse $bottom), (scalar reverse $left));
    my @flipped_signature = ((scalar reverse $top),
                             $left,
                             $bottom,
                             (scalar reverse $right));

    return \@signature, \@flipped_signature;
}


sub crop {
    my $tile = shift;
    shift @$tile;
    pop @$tile;
    for (@$tile) {
        my @row = split '', $_;
        shift @row;
        pop @row;
        $_ = join '', @row;
    }
    return $tile;
}

sub flip {
    my $tile = shift;

    for (@$tile) {
        $_ = reverse $_;
    }

    return $tile;
}

sub rotate {
    my $tile = shift;
    my $times = shift;

    for (@$tile) {
        $_ = [split '', $_];
    }
    for (1..$times) {
        my $new_tile;
        my @tile = @$tile;
        for my $y (0..$#tile) {
            for my $x (0..$#tile) {
                # y becomes x, max_y-x becomes y
                $new_tile->[$#tile - $x]->[$y] = $tile->[$y]->[$x];
            }
        }
        $tile = $new_tile;
    }

    for (@$tile) {
        $_ = join '', @$_;
    }
    return $tile;
}
