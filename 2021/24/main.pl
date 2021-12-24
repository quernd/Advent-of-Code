#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

sub execute {
    my ($input, $instructions, $register) = @_;
    my %register = %{$register};
    my @input = @{$input};
    for (@{$instructions}) {
        if (/inp ([wxyz])/) {
            die "Input exhausted." unless @input;
            $register{$1} = shift @input;
        }
        elsif (/add ([wxyz]) ([wxyz]|-?\d+)/) {
            $register{$1} = $register{$1} + ($register{$2} // $2);
        }
        elsif (/mul ([wxyz]) ([wxyz]|-?\d+)/) {
            $register{$1} = $register{$1} * ($register{$2} // $2);
        }
        elsif (/div ([wxyz]) ([wxyz]|-?\d+)/) {
            use integer;
            return undef if $register{$2} // $2 == 0;
            $register{$1} = $register{$1} / ($register{$2} // $2);
        }
        elsif (/mod ([wxyz]) ([wxyz]|-?\d+)/) {
            return undef if $register{$1} < 0 or $register{$2} // $2 <= 0;
            $register{$1} = $register{$1} % ($register{$2} // $2);
        }
        elsif (/eql ([wxyz]) ([wxyz]|-?\d+)/) {
            $register{$1} = ($register{$1} == ($register{$2} // $2) ? 1 : 0);
        }
        else {
            die "Illegal instruction $_";
        }
    }
    return \%register;
}

my @instructions = <>;
my %register = (x => 0, y => 0, z => 0, w => 0);

# Solved by hand, this is just for verification
for my $solution ('74929995999389', '11118151637112') {
    my $result = execute([split '', $solution], \@instructions, \%register);
    if ($result->{z} == 0) {
        say $solution;
    }
    else {
        say "Not the solution."
    }
}