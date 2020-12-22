#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use Data::Dumper;

local $/ = q();

my @hands;

while (my $hand = <>) {
    my @cards = split /\n/, $hand;
    shift @cards;
    push @hands, \@cards;
}

for (;;) {
    my ($hand0, $hand1) = @hands;
    last unless @{$hand0} and @{$hand1};

    #warn "@$hand0";
    #warn "@$hand1";
    my $card0 = shift @$hand0;
    my $card1 = shift @$hand1;
    
    if ($card0 > $card1) {
        push @$hand0, $card0, $card1;
    }
    else {
        push @$hand1, $card1, $card0;
    }

}

my @all_cards = (@{$hands[0]}, @{$hands[1]});
#warn "@all_cards";
my $sum = 0;

for (0..$#all_cards) {
    my $multiplier = @all_cards - $_;
    $sum += $all_cards[$_] * $multiplier;
    #warn $all_cards[$_], " * ", $multiplier;
}

say $sum;
