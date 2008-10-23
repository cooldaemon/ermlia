#!/usr/bin/env perl

use strict;
use warnings;

use Path::Class;

use FindBin qw($Bin);
use File::Spec;
use lib File::Spec->catfile($Bin, 'lib');

use ErmliaClient;

my $ec = ErmliaClient->new;
my $put_res = $ec->put(test => 'test message.');
print 'put code: ', $put_res->code, "\n";

my $head_res = $ec->head('test');
print 'head code: ', $head_res->code, "\n";

my $get_res = $ec->get('test');
print 'get content: ', $get_res->content, "\n";

