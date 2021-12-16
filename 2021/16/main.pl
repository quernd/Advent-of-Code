#!/usr/bin/env perl

use strict;
use warnings;
use v5.10;

use List::Util qw(min max sum product);

my %hex = (
    0 => '0000',
    1 => '0001',
    2 => '0010',
    3 => '0011',
    4 => '0100',
    5 => '0101',
    6 => '0110',
    7 => '0111',
    8 => '1000',
    9 => '1001',
    A => '1010',
    B => '1011',
    C => '1100',
    D => '1101',
    E => '1110',
    F => '1111',
);

my $transmission = <>;
chomp $transmission;
my @bits = map { split '', $hex{$_} } (split '', $transmission);

sub bin_to_dec {
    my $dec = 0;
    for (@_) {
        $dec = $dec * 2 + $_;
    }
    return $dec;
}

sub parse_literal {
    my @seq = @{shift @_};
    my @literal = ();
    while (1) {
        my @group = splice @seq, 0, 5;
        my $keep_going = shift @group;
        push @literal, @group;
        last unless $keep_going;
    }   
    return (bin_to_dec(@literal), \@seq);
}

sub parse_operator {
    my @seq = @{shift @_};
    my @sub_packets;
    my $length_indicator = shift @seq;
    my $read = 0;
    if ($length_indicator eq '0') {
        # 15 length in bits
        my $length = join '', (splice @seq, 0, 15);
        my $end_at = @seq - eval("0b$length");
        while (@seq > $end_at) {
            my ($packet, $rest) = parse_packet(\@seq);
            push @sub_packets, $packet;
            @seq = @{$rest};
        }
    }
    else {
        # 11 number of packets
        my $number = join '', (splice @seq, 0, 11);
        $number = eval("0b$number");
        while ($read < $number) {
            my ($packet, $rest) = parse_packet(\@seq);
            push @sub_packets, $packet;
            $read++;
            @seq = @{$rest};
        }
    }
    return \@sub_packets, \@seq;
}

sub parse_packet {
    my @seq = @{shift @_};
    my @version = splice @seq, 0, 3;
    my @type_id = splice @seq, 0, 3;
    my $type_id = join '', @type_id;

    my $rest;
    my $literal;
    my $sub_packets;

    if ($type_id eq '100') { # literal
        ($literal, $rest) = parse_literal(\@seq);
    }
    else {
        ($sub_packets, $rest) = parse_operator(\@seq);
    }

    return ({
        version => bin_to_dec(@version),
        type_id => bin_to_dec(@type_id),
        literal => $literal,
        sub_packets => $sub_packets,
    }, $rest)
}


sub version_sum {
    my $packet = shift;
    my $version = $packet->{version};
    for my $sub_packet (@{$packet->{sub_packets}}) {
        $version += version_sum($sub_packet);
    }
    return $version;
}

sub evaluate {
    my $packet = shift;
    my $type_id = $packet->{type_id};

    if ($type_id == '0') {
        return sum(map { evaluate($_) } @{$packet->{sub_packets}});
    }
    elsif ($type_id == '1') {
        return product(map { evaluate($_) } @{$packet->{sub_packets}});
    }    
    elsif ($type_id == '2') {
        return min(map { evaluate($_) } @{$packet->{sub_packets}});
    }
    elsif ($type_id == '3') {
        return max(map { evaluate($_) } @{$packet->{sub_packets}});
    }
    elsif ($type_id == '4') {
        return $packet->{literal};
    }
    elsif ($type_id == '5') {
        return evaluate($packet->{sub_packets}->[0]) > evaluate($packet->{sub_packets}->[1]) ? 1 : 0;
    }
    elsif ($type_id == '6') {
        return evaluate($packet->{sub_packets}->[0]) < evaluate($packet->{sub_packets}->[1]) ? 1 : 0;
    }
    elsif ($type_id == '7') {
        return evaluate($packet->{sub_packets}->[0]) eq evaluate($packet->{sub_packets}->[1]) ? 1 : 0;
    }
}

my ($parsed, $rest) = parse_packet(\@bits);
say version_sum($parsed);
say evaluate($parsed);
