#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

#my @lines;

my %passport = ();

my $result = 0;

while (my $line = <>) {
    chomp $line;
    if ($line) {
        while ($line =~ /([^: ]+):([^: ]+)/g) {
            $passport{$1} = $2;
        }
    }
    else {
        #use Data::Dumper; warn Dumper \%passport;
        $result += 1 unless
            (grep { !defined $_ }
             @passport{qw/byr iyr eyr hgt hcl ecl pid/});
        %passport = ();
    }
}

$result += 1 unless
    (grep { !defined $_ }
     @passport{qw/byr iyr eyr hgt hcl ecl pid/});

say $result;
