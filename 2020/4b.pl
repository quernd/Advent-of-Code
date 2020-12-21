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
        $result += 1 if validate_passport(\%passport);
        %passport = ();
    }
}

$result += 1 if validate_passport(\%passport);

say $result;



sub validate_passport {
    my $passport = shift;
    my %passport = %$passport;
    return 0 if
        (grep { !defined $_ }
         @passport{qw/byr iyr eyr hgt hcl ecl pid/});
    return 0 unless $passport->{byr} =~ /^\d{4}$/ and $passport->{byr} >= 1920 and $passport->{byr} <= 2002;
    return 0 unless $passport->{iyr} =~ /^\d{4}$/ and $passport->{iyr} >= 2010 and $passport->{iyr} <= 2020;
    return 0 unless $passport->{eyr} =~ /^\d{4}$/ and $passport->{eyr} >= 2020 and $passport->{eyr} <= 2030;
    return 0 unless ($passport->{hgt} =~ /^(\d+)cm$/ and $1 >= 150 and $1 <= 193 or $passport->{hgt} =~ /^(\d+)in$/ and $1 >= 59 and $1 <= 76);
    return 0 unless $passport->{hcl} =~ /^\#[0-9a-f]{6}$/;
    return 0 unless $passport->{ecl} =~ /^amb|blu|brn|gry|grn|hzl|oth$/;
    return 0 unless $passport->{pid} =~ /^\d{9}$/;
    return 1;
}
