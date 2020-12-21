#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use List::Util qw(any all product);

my $sum = 0;

while (my $expression = <>) {
    chomp $expression;
    my @symbols = grep /[^ ]/, split //, $expression;
    my @stack;
    for my $symbol (@symbols) {
        if ($symbol =~ /\d/) {
            push @stack, $symbol;
        }
        elsif ($symbol =~ /[+*]/) {
            push @stack, $symbol;
        }
        elsif ($symbol =~ /\(/) {
            push @stack, $symbol;
        }
        elsif ($symbol =~ /\)/) {
            push @stack, $symbol;
        }
        else {
            die "Parsing error."
        }
      REDUCE: for (;;) {
          last unless @stack >= 3;
          # reduce if possible;
          if ($stack[-1] =~ /\d+/ and $stack[-2] =~ /[+*]/ and $stack[-3] =~ /\d+/) {
              my $rhs = pop @stack;
              my $op  = pop @stack;
              my $lhs = pop @stack;
              push @stack, eval("$lhs $op $rhs");
          }
          elsif ($stack[-1] =~ /\)/ and $stack[-2] =~ /\d+/ and $stack[-3] =~ /\(/) {
              pop @stack;
              my $value = pop @stack;
              pop @stack;
              push @stack, $value;
          }
          else {
              last REDUCE;
          }
      }
        
    }
   # warn "@stack";
    $sum += $stack[0];
}

say $sum;
