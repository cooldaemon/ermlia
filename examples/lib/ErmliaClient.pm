package ErmliaClient;

use strict;
use warnings;

use version; our $VERSION = qv('0.0.1');

use LWP::UserAgent;

use Readonly;
Readonly my $DEFAULT_URI    => 'http://localhost:10000/';
Readonly my $DEFAULT_TYPE   => 'text/plain';
Readonly my $DEFAULT_EXPIRE => 0;

for my $sub_name (qw(get head)) {
    no strict 'refs';
    *{__PACKAGE__ . q{::} . $sub_name} = sub {
        my $self = shift;
        my ($key) = @_;
        return $self->_request(uc $sub_name => $key);
    };
}

sub new {
    my $class = shift;
    my ($uri) = @_;

    return bless {
        uri => $uri || $DEFAULT_URI,
        ua  => LWP::UserAgent->new(parse_head => 0),
        @_
    }, $class;
}

sub put {
    my $self = shift;
    my ($key, $value, $type, $expire) = @_;

    my $req = HTTP::Request->new(PUT => $self->{uri} . $key);
    $req->content_type($type || $DEFAULT_TYPE);
    $req->header('x-ermlia-expire' => $expire || $DEFAULT_EXPIRE);
    $req->content($value);

    return $self->{ua}->request($req);
}

sub _request {
    my $self = shift;
    my ($method, $key,) = @_;

    return $self->{ua}->request(
        HTTP::Request->new($method => $self->{uri} . $key)
    );
}

1;

