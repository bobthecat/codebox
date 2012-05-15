#! /usr/bin/perl -w
#
# sex_diff_pain.pl
#
# Created by David Ruau on 2011-02-17.  
# Department of Pediatrics/Div. System Medicine Stanford University.
# 
##################### USAGE #########################
# 
# Query PubMed with Eutils tools
# 
#####################################################
use Bio::TGen::EUtils;

use strict;

my $queryString = $ARGV[0];

## query info
my $eu = Bio::TGen::EUtils->new( 'tool'  => 'pubmed_trend.pl',
                                 'email' => 'REPLACE_ME@gmail.com' );

## EFetch
my $query = $eu->esearch( db   => 'pubmed',
                          term => $queryString,
                          usehistory => 'n' );

$query->write_raw( file => 'tempfile.xml' );

if (-z 'tempfile.xml') {
  # one more time
  my $query = $eu->esearch( db   => 'pubmed',
                            term => $queryString,
                            usehistory => 'n' );

  $query->write_raw( file => 'tempfile.xml' );
  if (-z 'tempfile.xml') {
    open (FILE, '>', 'tempfile.xml') or die 'Could not open file, $!';
    
    print FILE "<begin>hello world</begin>";
    close (FILE);
  }
}