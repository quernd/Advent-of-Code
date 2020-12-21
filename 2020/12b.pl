#!/usr/bin/perl
use v5.10;
use strict;
use warnings;


my $x = 0;
my $y = 0;

my $waypoint_x = 10;
my $waypoint_y = -1;

sub rotate {
    my $degrees = shift;
    my ($wp_x, $wp_y) = ($waypoint_x, $waypoint_y);
    for (1..$degrees / 90) {
        ($wp_x, $wp_y) = (-$wp_y, $wp_x);
    }
    return ($wp_x, $wp_y);
}

while (my $line = <>) {
    chomp $line;
    $line =~ /([NSEWLRF])(\d+)/;
    my $direction;
    if ($1 eq 'N') {
        $waypoint_y -= $2;
    }
    elsif ($1 eq 'E') {
        $waypoint_x += $2;
    }
    elsif ($1 eq 'S') {
        $waypoint_y += $2;
    }
    elsif ($1 eq 'W') {
        $waypoint_x -= $2;
    }
    elsif ($1 eq 'L') {
        ($waypoint_x, $waypoint_y) = rotate(360 - $2);
        next;
    }
    elsif ($1 eq 'R') {
        ($waypoint_x, $waypoint_y) = rotate($2);
        next;
    }
    elsif ($1 eq 'F') {
        $x += $2 * $waypoint_x;
        $y += $2 * $waypoint_y;
    }

}

say (abs($x) + abs($y));

