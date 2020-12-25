#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use Data::Dumper;

my $MAGIC_NUMBER = 20201227;
my $INITIAL_SUBJECT = 7;

my $card_public = <>; chomp $card_public;
my $door_public = <>; chomp $door_public;

sub step {
    my ($value, $subject_number) = @_;
    return ($value * $subject_number) % $MAGIC_NUMBER;
}

sub crack_loop_size {
    my ($subject_number, $key) = @_;
    my $value = 1;

    my $loop_size = 0;
    
    while ($value != $key) {
        $value = step($value, $subject_number);
        $loop_size += 1;
    }

    return $loop_size;
}

sub encrypt {
    my ($subject_number, $loop_size) = @_;
    my $value = 1;

    $value = step($value, $subject_number) for (1..$loop_size);

    return $value;
}

my $card_loop_size = crack_loop_size($INITIAL_SUBJECT, $card_public);
my $door_loop_size = crack_loop_size($INITIAL_SUBJECT, $door_public);

# For a sanity check, both need to be the same
say encrypt($card_public, $door_loop_size);
say encrypt($door_public, $card_loop_size); 
