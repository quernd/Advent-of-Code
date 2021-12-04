#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(sum);

sub make_board {
    my $input = shift;
    my @lines = split("\n", $input);
    my @board = map {[split ' ', $_]} @lines;
    return \@board;
}

sub mark_board {
    my ($board, $picked) = @_;
    for my $line (@$board) {
        for my $number (@$line) {
            $number = undef if (defined $number and $number eq $picked);
        }
    }
}

sub check_board {
    my $winning = 0;
    my $board = shift;
    my $max_column = $#{$board->[0]};
    for my $line (@$board) {
        $winning = 1 unless (grep {defined} @$line);
    }
    for my $column (0..$max_column) {
        my @column = map {$board->[$_]->[$column]} (0..$max_column);
        $winning = 1 unless (grep {defined} @column);
    }
    if ($winning) {
        my $sum = 0;
        for my $line (@$board) {
            my @unmarked = grep {defined} @$line;
            $sum += sum(@unmarked) if @unmarked;
        }
        return $sum;
    }
    return undef;
}

local $/ = "\n\n";

my @numbers = split(",", <>);
my @boards = map {make_board($_)} <>;

my %winning;
my @winners;

for my $number (@numbers) {
    for my $board (@boards) {
        unless ($winning{$board}) {
            mark_board($board, $number);
            my $check = check_board($board);
            if (defined $check) {
                $winning{$board} = 1;
                push @winners, ($check * $number);
            }
        }
    }
}

say "Part 1: ", $winners[0];
say "Part 2: ", $winners[$#winners];
