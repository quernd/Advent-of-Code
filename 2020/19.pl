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
                $lexical_rules{$1}->{$lhs} = 1;
            }
            elsif ($rhs =~ /^\d+$/) {
                $unary_rules{$rhs}->{$lhs} = 1;
            }
            elsif ($rhs =~ /^(\d+) (\d+)$/) {
                $binary_rules{$1}->{$2}->{$lhs} = 1;
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
        for my $nonterminal (keys %{$lexical_rules{$string[$i]}}) {
            $nonterminals[$i]->[$i]->{$nonterminal} = 1
        }
    }

    # Simple CYK parser for binary grammars
    # Iterate over successively longer spans, looking at each possible split

    for my $length (1..$#string + 1) {
        for my $start (0..$#string + 1 - $length) {
            my $start = $start;
            my $end = $start + $length - 1;
            for my $split (1..$length - 1) {
                my $f_end = $start + $split - 1;
                my $s_start = $start + $split;
                my $f_nonterminals = $nonterminals[$start]->[$f_end];
                my $s_nonterminals = $nonterminals[$s_start]->[$end];
                for my $first (keys %$f_nonterminals) {
                    for my $second (keys %{$binary_rules{$first}}) {
                        next unless $s_nonterminals->{$second};
                        $nonterminals[$start]->[$end]->{$_} = 1
                            for keys %{$binary_rules{$first}->{$second}};
                    }
                }
                                       
            }

            # Processing of unary (chain) rules if needed;
            next unless $nonterminals[$start]->[$end];

            my $num = scalar keys %{$nonterminals[$start]->[$end]};
            for (;;) {
                for my $rhs (keys %unary_rules) {
                    for my $lhs (keys %{$unary_rules{$rhs}}) {
                        next unless $unary_rules{$rhs}->{$lhs};
                        $nonterminals[$start]->[$end]->{$lhs} = 1
                            if $nonterminals[$start]->[$end]->{$rhs};
                    }
                }
                my $new_num = scalar keys %{$nonterminals[$start]->[$end]};
                last unless $new_num > $num;
                $num = $new_num;
            }
        }
    }
    $valid += $nonterminals[0]->[$#string]->{0} // 0;
}
    
    
say $valid;
