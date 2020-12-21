#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(any all product);

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

my @valid_tickets;
my @compatible;

push @compatible, { map {$_ => 1} (0..$#rules) } for @rules;

for (1..$#tickets) { # ignore my own ticket
    my @ticket = @{$tickets[$_]};
    # each field has to conform to all rules
    for my $field_id (0..$#ticket) {
        my $field = $ticket[$field_id];
        if (any {$_->($field)} @rules) {
            push @valid_tickets, \@ticket;
            for my $rule_id (0..$#rules) {
                delete $compatible[$rule_id]->{$field_id}
                    unless $rules[$rule_id]->($field);
            }
        }
        else {
            $sum += $field;
        }
    }
}

my %assignment;
while (keys %assignment < @compatible) {
    my $key;
  F: for (0..$#compatible) {
      my $field = $compatible[$_];
      if (keys %$field == 1) {
          $key = (keys %$field)[0];
          $assignment{$_} = $key;
          last F;
      }
  }
    for my $field (@compatible) {
        delete $field->{$key};
  }
    #say $key;
}

#use Data::Dumper;
#say Dumper \%assignment;
#say "@{$tickets[0]}";

my $part2 = product (map {$tickets[0]->[$assignment{$_}]} (0..5));

say $part2;

