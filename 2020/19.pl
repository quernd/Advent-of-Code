#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

my %unary_rules;
my %binary_rules;
my %lexical_rules;

while (my $rule = <>) {
    chomp $rule;
    last unless $rule;
    if ($rule =~ /^(\d+): (.*)$/) {
        my $lhs = $1;
        
        for my $rhs (split / \| /, $2) {
            
            if ($rhs =~ /^"(\w)"$/) {
                $lexical_rules{$lhs}->{$1} = 1;
            }
            elsif ($rhs =~ /^\d+$/) {
                $unary_rules{$lhs}->{$rhs} = 1;
            }
            elsif ($rhs =~ /^\d+ \d+$/) {
                $binary_rules{$lhs}->{$rhs} = 1;
            }
            else {
                die "Parse error: $rule";
            }
        }
    }
}

my $valid = 0;

while (my $string = <>) {
    chomp $string;
    my @string = split //, $string;
    my @nonterminals;

    for my $i (0..$#string) {
        for my $nonterminal (keys %lexical_rules) {
            $nonterminals[$i]->[$i]->[$nonterminal] = 1
                if $lexical_rules{$nonterminal}->{$string[$i]};
        }
        #warn "$i:$i";
    }
    #use Data::Dumper; warn Dumper \%nonterminals;    
    for my $length (1..$#string + 1) {
        for my $start (0..$#string + 1 - $length) {
            my $lhs1 = $start;
            my $rhs2 = $start + $length - 1;
            for my $split (1..$length - 1) {
                my $rhs1 = $start + $split - 1;
                my $lhs2 = $start + $split;
                for my $lhs (keys %binary_rules) {
                    #warn "$lhs1:$rhs1 + $lhs2:$rhs2";
                    for my $rhs (keys %{$binary_rules{$lhs}}) {
                        next unless $binary_rules{$lhs}->{$rhs};
                        my ($l, $r) = split / /, $rhs;
                        $nonterminals[$lhs1]->[$rhs2]->[$lhs] = 1
                            if $nonterminals[$lhs1]->[$rhs1]->[$l]
                        and $nonterminals[$lhs2]->[$rhs2]->[$r];
                    }
                }
                                       
            }

            my $num = grep {defined} $nonterminals[$lhs1]->[$rhs2];
            for (;;) {
                for my $lhs (keys %unary_rules) {
                    for my $rhs (keys %{$unary_rules{$lhs}}) {
                        next unless $unary_rules{$lhs}->{$rhs};
                        $nonterminals[$lhs1]->[$rhs2]->[$lhs] = 1
                            if $nonterminals[$lhs1]->[$rhs2]->[$rhs];
                    }
                }
                my $new_num = grep {defined} $nonterminals[$lhs1]->[$rhs2];
                last unless $new_num > $num;
                $num = $new_num;
            }
                                    
            #warn "$lhs1:$rhs2 chain rules";
        }
    }
    #warn ".";
    $valid += $nonterminals[0]->[$#string]->[0] // 0;
}
    
    
say $valid;
