#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(any all);

my $sum;
my @rules;

while (my $rule = <>) {
    #say $rule;
    chomp $rule;
    last unless $rule;
    if ($rule =~ /(\d+)-(\d+) or (\d+)-(\d+)/) {
        my ($l1, $u1, $l2, $u2) = ($1, $2, $3, $4);
        #say "$l1, $u1, $l2, $u2";
        
        push @rules, sub {
            my $field = shift;
            #say "$l1, $u1, $l2, $u2, $field";
            return ($field >= $l1 and $field <= $u1
                    or $field >= $l2 and $field <= $u2);

        };
    }
    else {
        say $rule;
    }
}

my @tickets;

while (my $line = <>) {
    chomp $line;
    next unless $line;
    next if $line =~ /ticket/;
    push @tickets, [split /,/, $line];
}

for (1..$#tickets) { # ignore my own ticket
    my @ticket = @{$tickets[$_]};
    # each field has to conform to all rules
    for my $field (@ticket) {
        $sum += $field unless any {$_->($field)} @rules;
    }
}

say $sum;
