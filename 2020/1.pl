#!/usr/bin/perl
use v5.10;
use strict;
use warnings;

my @list = <>;

for my $first (0..$#list) {
	for my $second ($first+1..$#list) {
		if ($list[$first] + $list[$second] == 2020) {
			say $list[$first] * $list[$second];
		}
		for my $third ($second+1..$#list) {
			if ($list[$first] + $list[$second] + $list[$third] == 2020) { 
				say $list[$first] * $list[$second] * $list[$third];
				exit 
			}
		}
	}
}
