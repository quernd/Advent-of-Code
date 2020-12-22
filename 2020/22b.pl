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

sub play_game {
    my $hand0 = shift;
    my $hand1 = shift;
    my $level = shift;
    my %seen;
    for (;;) {
        #say "$level @$hand0 ; @$hand1";
        
        if ($seen{"@$hand0:@$hand1"}) {
            return 0;
        }
        else {
            $seen{"@$hand0:@$hand1"} = 1;
        }
        
        my $card0 = shift @$hand0;
        my $card1 = shift @$hand1;

        my $winner;
        
        if (@$hand0 >= $card0 and @$hand1 >= $card1) {
            #say "$level Recurse!";
            # recurse
            my @copy0 = @{$hand0}[0..$card0 - 1];
            my @copy1 = @{$hand1}[0..$card1 - 1];
            $winner = play_game(\@copy0, \@copy1, $level."\t");
            #say "$level Back with winner $winner";
        }
        else {
            if ($card0 < $card1) {
                #say "$level Player 1 dealt higher card.";
                $winner = 1;
            }
            else {
                #say "$level Player 0 dealt higher card.";
                $winner = 0;
            }
        }

        # add cards to winner's deck

        if ($winner) {
            push @$hand1, $card1, $card0;
            return 1 unless @$hand0;
        }
        else {
            push @$hand0, $card0, $card1;
            return 0 unless @$hand1;
        }

    }

}

say play_game(@hands, '');

my @all_cards = (@{$hands[0]}, @{$hands[1]});
#warn "@all_cards";
my $sum = 0;

for (0..$#all_cards) {
    my $multiplier = @all_cards - $_;
    $sum += $all_cards[$_] * $multiplier;
    #warn $all_cards[$_], " * ", $multiplier;
}

say $sum;
