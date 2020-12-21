#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my %hash;

while (my $line = <>) {
    $line =~ /^(\w+ \w+) bags contain (.*)$/;
    my $lhs = $1;
    my $rest = $2;
    while ($rest =~ /(\d+) (\w+ \w+) bag/g) {
        my $number = $1;
        my $rhs = $2;
        push @{$hash{$lhs}}, [$number, $rhs];
    }
    
}



say (number('shiny gold') - 1);

sub number {
    my $color = shift;
    my $rhs = $hash{$color};
    #use Data::Dumper; warn Dumper $rhs;
    return 1 unless defined $rhs;
    my $counter = 1;
    for (@$rhs) {
        my ($number, $ccolor) = @$_;
        $counter += $number * number($ccolor);
    }
    return $counter;
}
