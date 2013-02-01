# set timestamp_timeout to 1 in order to use this script                                

use Irssi;
use POSIX qw(strftime);
use strict;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";
%IRSSI = (
	authors     => 'ellipsis',
	name        => 'redundancy',
	description => "Based on Antti Ruokomäki's wisestamp, it hides" .
	'redundant timestamps',
	license     => 'Public Domain',
	changed     => 'Wed Dec  8 09:59:30 2010'
);


my $inprogress = 0; # $inprogress prevents infinite printint loops
my $indent;         # indent string for the replacement
my $format;         # irssi timestamp format

# The main function
sub show_stamp_shadow {                                                                 

	return if ($inprogress || Irssi::settings_get_str('timestamp_timeout') == 0);

	$inprogress = 1;

	my ($destination, $text, $stripped) = @_;
	my $last = strftime $format, localtime($destination->{window}->{last_timestamp});
	my $current = strftime $format, localtime(time());
	# if the timestamp changed, display it
	if ($last ne $current) {
		Irssi::settings_set_str('timestamps', "ON");
		Irssi::signal_emit('setup changed');
		$inprogress = 0;
		return
	}
	Irssi::settings_set_str('timestamps', "OFF");
	Irssi::signal_emit('setup changed');

	$text = $indent.$text;

	# Output the manipulated text
	Irssi::signal_emit('print text', $destination, $text, $stripped);
	$inprogress = 0;
	Irssi::signal_stop();
}

sub init {
	$format = Irssi::settings_get_str('timestamp_format');
	$indent = " ".$format;
	$indent =~ s/./ /g
}

Irssi::signal_add('print text'    , \&show_stamp_shadow);
init();
