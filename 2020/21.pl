#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;
use Data::Dumper;

my @foods;
my %ingredients;
my %allergen_map;

while (my $food = <>) {
    $food =~ /^([^\(]+) \(contains ([^\)]+)\)$/ or die "Malformed input: $food";
    my $contains = $1;
    my $contains_allergens = $2;
    #say $contains;
    my @contains = split / /, $contains;
    my @contains_allergens = split /, /, $contains_allergens;
    #print Dumper \@contains;
    push @foods, { contains => { map {$_ => 1} @contains},
                   ingredients => \@contains,
                   contains_allergens =>
                       { map {$_ => 1} @contains_allergens} };

    $ingredients{$_}  = 0  for @contains;
    $allergen_map{$_} = {} for @contains_allergens;
}

#print Dumper \%ingredients;

for my $ingredient (keys %ingredients) {
    for my $allergen (keys %allergen_map) {
        $allergen_map{$allergen}->{$ingredient} = 1; # still possible
    }
}

say join ' ', keys %allergen_map;

my %found_allergens;
my %found_ingredients;

while (keys %found_allergens < keys %allergen_map) {
    for my $food (@foods) {
        
        #use Data::Dumper; print Dumper \%allergen_map;
        my $allergens = $food->{contains_allergens};
        my $contains = $food->{contains};
        # remove known allergens
        for (keys %$allergens) {
            delete $allergens->{$_} if $found_allergens{$_};
        }
        for (keys %$contains) {
            delete $contains->{$_} if $found_ingredients{$_};
        }
        
        for my $allergen (keys %$allergens) {
            for my $possible_ingredient (keys %{$allergen_map{$allergen}}) {
                delete $allergen_map{$allergen}->{$possible_ingredient} unless $contains->{$possible_ingredient};
            }
            if (keys %{$allergen_map{$allergen}} == 1) {
                my $problematic_ingredient = (keys %{$allergen_map{$allergen}})[0];
                say "Allergen found: $problematic_ingredient contains $allergen";
                $found_allergens{$allergen} = $problematic_ingredient;
                $found_ingredients{$problematic_ingredient} = $allergen;
                #use Data::Dumper; print Dumper %{$allergen_map{$allergen}};
            }
        }
    }        

}


my $sum = 0;

for my $food (@foods) {
    my $ingredients = $food->{ingredients};
    my @good_ingredients = grep {!$found_ingredients{$_}} @$ingredients;
    $sum += @good_ingredients;
}

say $sum;


my @sorted_allergens = sort keys %found_allergens;
my @canonical_ingredients = map {$found_allergens{$_}} @sorted_allergens;

say join ',', @canonical_ingredients;
