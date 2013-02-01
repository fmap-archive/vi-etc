use strict;
use Irssi qw{command_bind command_runsub get_irssi_dir};
use vars qw{$VERSION %IRSSI};

$VERSION = "0.0.1";
%IRSSI = (
  name        => 'password.pl',
  contact     => 'me@vikramverma.com',
  description => 'Handles NickServ authentication, ' .
                 'based on `cap_sasl.pl`.',
  license     => 'GNU General Public License',
);

my %auth = ();

command_bind "password" => sub {
  my ($data, $server, $witem) = @_;
  unless ($data eq '') {
    command_runsub ('password', $data, $server, $witem);
  }
};

command_bind 'password set' => sub {
  my($data, $server, $witem) = @_;
  if (my($network, $user, $password)  = $data =~ /^(\S+) (\S+) (\S+)$/) {
    $auth{$network}{user}     = $user;
    $auth{$network}{password} = $password;
    Irssi::print("password.pl: added password for $auth{$network}{user} at $network.");
  } else {
    Irssi::print("password.pl: /password set <NETWORK> <USER> <PASSWORD>");
  }
};

command_bind "password save" => sub {
	my $file = Irssi::get_irssi_dir()."/../.security/irssi/password.pass";
  open FILE, "> $file" or return;
  foreach my $network (keys %auth) {
    printf FILE ("%s\t%s\t%s\n", $network, $auth{$network}{user}, $auth{$network}{password});
  }
  close FILE;
  Irssi::print("password.pl: password saved to $file");
};

command_bind "password send" => sub {
  my($data, $server, $witem) = @_;
  my $user = $auth{$server->{tag}}{user};
  my $password = $auth{$server->{tag}}{password};
  $server->command("msg NickServ identify $user $password"); 
};

sub init {
  my $file = get_irssi_dir()."/../.security/irssi/password.pass";
  open FILE, "< $file" or return;
  %auth = ();
  while (<FILE>) {
    chomp;
    my ($network, $user, $password) = split (/\t/, $_, 3);
    $auth{$network}{user} = $user;
    $auth{$network}{password} = $password; 
  };
  close FILE;
  Irssi::print("password.pl: password loaded from $file");
};

init();
