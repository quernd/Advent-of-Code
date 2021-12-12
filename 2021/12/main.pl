#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(any);

my $PART1 = 0;

my %caves;
my @queue;

while (<>) {
    if (/([a-zA-Z]+)-([a-zA-Z]+)/) {
        $caves{$1}->{$2} = 1;
        $caves{$2}->{$1} = 1;
    }
}

push @queue, { pos => "start", path => "start" };

my %paths;

while (my $state = shift @queue) {
    my $pos = $state->{pos};
    my $path = $state->{path};

    my %visited;
    for my $cave (split ',', $path) {
        $visited{$cave}++ if $cave =~ /[a-z]/;
    }

    $paths{$pos}++;

    if (defined $caves{$pos}) {
        my %connections = %{$caves{$pos}};
        for my $next (keys %connections) {
            unless ($next eq "start" or $pos eq "end" or
                    ($next =~ /[a-z]/ and $visited{$next}
                     and ($PART1 or (any { ($visited{$_} // 0) > 1 } (keys %visited)))
                    )
                ) {
                push @queue, { pos => $next, path => "$path,$next"};
            }
        }
    }
}

say $paths{end};