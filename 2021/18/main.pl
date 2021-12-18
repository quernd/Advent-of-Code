#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use Data::Dumper;
$Data::Dumper::Indent = 0;

sub snailfish_add {
    my ($left, $right) = @_;
    return [$left, $right];
}

sub snailfish_split {
    my $snailfish = shift;
    if (ref $snailfish eq 'ARRAY') {
        my ($left, $right) = @{$snailfish};
        my ($success, $result) = snailfish_split($left);
        return ($success, [$result, $right]) if $success;
        ($success, $result) = snailfish_split($right);
        return ($success, [$left, $result]) if $success;
        return (0, $snailfish);
    }
    else {
        if ($snailfish >= 10) {
            my $left = int($snailfish / 2);
            my $right = $snailfish - $left;
            return (1, [$left, $right]);
        }
        else {
            return (0, $snailfish);
        }
    }
}

sub add_to_leftmost {
    my ($snailfish, $add) = @_;
    if (ref $snailfish eq 'ARRAY') {
        my ($left, $right) = @{$snailfish};
        return [add_to_leftmost($left, $add), $right];
    }
    else {
        return $snailfish + $add;
    }
}

sub add_to_rightmost {
    my ($snailfish, $add) = @_;
    if (ref $snailfish eq 'ARRAY') {
        my ($left, $right) = @{$snailfish};
        return [$left, add_to_rightmost($right, $add)];
    }
    else {
        return $snailfish + $add;
    }
}

sub snailfish_explode {
    my ($snailfish, $depth) = @_;
    if (ref $snailfish eq 'ARRAY') {
        my ($left, $right) = @{$snailfish};
        if ($depth >= 4 and ref $left ne 'ARRAY' and ref $right ne 'ARRAY') {
            return (1, 0, $left, $right);
        }
        else {
            my ($success, $result, $l, $r) = snailfish_explode($left, $depth + 1);
            if ($success) {
                if ($r) {
                    $right = add_to_leftmost($right, $r);
                    $r = 0;
                }
                return ($success, [$result, $right], $l, $r);
            }
            ($success, $result, $l, $r) = snailfish_explode($right, $depth + 1);
            if ($success) {
                if ($l) {
                    $left = add_to_rightmost($left, $l);
                    $l = 0;
                }
                return ($success, [$left, $result], $l, $r);
            }
            return (0, $snailfish, 0, 0);         
        }
    }
    else {
        return (0, $snailfish, 0, 0);
    }
}

sub snailfish_reduce {
    sub reduce_step {
        my $snailfish = shift;
        my ($success, $result) = snailfish_explode($snailfish, 0);
        return ($success, $result) if $success;
        ($success, $result) = snailfish_split($snailfish);
        return ($success, $result) if $success;   
        return (0, $snailfish);
    }

    my $snailfish = shift;
    my $reduce = 1;
    while ($reduce) {
        ($reduce, $snailfish) = reduce_step($snailfish);
    }
    return $snailfish;
}

my @snailfish;
while (<>) {
    push @snailfish, eval($_);
}

sub snailfish_sum {
    my $sum = undef;
    for my $snailfish (@_) {
        $sum = defined $sum ?
            snailfish_reduce(snailfish_add($sum, $snailfish)) :
            $snailfish;
    }
    return $sum;
}

sub magnitude {
    my $snailfish = shift;
    if (ref $snailfish eq 'ARRAY') {
        my ($left, $right) = @{$snailfish};
        return 3 * magnitude($left) + 2 * magnitude($right);
    }
    else {
        return $snailfish;
    }
}

say magnitude(snailfish_sum(@snailfish));

my $max = 0;
for my $x (@snailfish) {
    for my $y (@snailfish) {
        my $sum = magnitude(snailfish_reduce(snailfish_add($x, $y)));
        if ($sum > $max) {
            $max = $sum;
        }
    }
}

say $max;