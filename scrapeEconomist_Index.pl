#!usr/bin/perl
use strict;
use warnings;
use LWP::Simple;
my $line;

open(indexURL, 'indexURLs.txt');
open(articleURL, '>articleURL.txt');

foreach $line (<indexURL>)  {   
    chomp($line);
    print "$line\n";    
	my $sou = get($line) or die "cannot retrieve code\n";
	my $date = substr $line, -10;  
				
		while ($sou =~ /\<a\shref\=\"\/news\/(\w*)\/(.*)\"\sclass\=\"node-link\"\>(.*)\<\/a\>\s*\<a\shref\=\"\/node\//g) 
		{    
			print articleURL "$date\thttp://www.economist.com/news/$1/$2\n";
		}
}


close(articleURL);




