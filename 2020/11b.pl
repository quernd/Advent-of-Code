#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my %hash;
my $row = 0;

while (my $line = <>) {
    $row += 1;
    chomp $line;
    my $column = 0;
    for (split //, $line) {
        $column += 1;
        $hash{"$row:$column"}->{type} = $_;
    }
}

#use Data::Dumper; warn Dumper \%hash;
#exit;

for (;;) {
    my $occupied = 0;
    my $changed = 0;
    for my $key (keys %hash) {
        my $seat = $hash{$key};
        $occupied += 1 if $seat->{type} eq '#';
        #use Data::Dumper; warn Dumper $seat;
        $key =~ /(\d+):(\d+)/;
        my $row = $1;
        my $column = $2;
        if ($seat->{type} eq '.') {

        }
        elsif ($seat->{type} =~ /[#L]/) {
            my $neighbors = 0;
            for my $up (-1, 0, 1) {
                for my $right (-1, 0, 1) {
                    next if ($right == 0 and $up == 0);
                    my $neighbor = '.';
                    my $roww = $row;
                    my $columnn = $column;
                  TRY: for (;;) {
                        $roww += $up;
                        $columnn += $right;
                        my $neighb = "$roww:$columnn";                        
                        last TRY unless exists $hash{$neighb};
                        $neighbor = $hash{$neighb}->{type};
                        last TRY unless $neighbor eq '.';
                    }
                    $neighbors += 1 if $neighbor eq '#';


                }
            }
            if ($seat->{type} eq '#' and $neighbors >= 5) {
                $changed += 1;
                $seat->{change_to} = 'L';
            }
            elsif ($seat->{type} eq 'L' and $neighbors == 0) {
                $changed += 1;
                $seat->{change_to} = '#';
            }
        }


    }

    if ($changed == 0) {
        say $occupied;
        exit;
    }
    else {
        for my $key (keys %hash) {
            $hash{$key}->{type} = $hash{$key}->{change_to}
            if $hash{$key}->{change_to};
        }
        #for my $r (1..11) {
        #    for my $c (1..10) {
        #        next unless exists $hash{"$r:$c"};
        #        print $hash{"$r:$c"}->{type};
        #    }
        #    print "\n";
        #}
    }
}
