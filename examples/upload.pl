#!/usr/bin/env perl

use strict;
use warnings;

use LWP::UserAgent;
use HTTP::Request;
use Data::Dumper;

my $req = HTTP::Request->new(
  'PUT',
  'http://localhost:10000/foo'
);
$req->content_type('text/plain');
$req->header('x-ermlia-expire' => 10);
$req->content('bar');

my $res = LWP::UserAgent->new->request($req);
print $res->status_line, "\n";
print $res->content, "\n";

