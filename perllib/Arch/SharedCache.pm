# Arch Perl library, Copyright (C) 2004 Mikhael Goikhman
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

use 5.005;
use strict;

package Arch::SharedCache;

use base 'Arch::SharedIndex';
use Arch::Util qw(save_file load_file);

sub new ($%) {
	my $class = shift;
	my %init = @_;

	my $dir = $init{dir} or die "No cache directory given\n";
	unless (-d $dir) {
		mkdir($dir, 0777) or die "Can't create cache directory $dir: $!\n";
	}
	-d $dir or die "No cache directory ($dir)\n";

	my $index_file = $init{index_file} || $init{file} || '.index';
	$index_file = "$dir/$index_file" unless $index_file =~ m!^\.?/!;

	my $self = $class->SUPER::new(
		# default to a more readable serialization output
		perl_data_indent => 1,
		perl_data_pair   => " => ",
		%init,
		file => $index_file,
	);
	$self->{dir} = $dir;
	$self->{generic_filenames} = $init{generic_filenames} || 0;

	return $self;
}

sub generate_unique_token ($) {
	my $self = shift;
	my $dir = $self->{dir};
	my $prefix = time() . "-";
	my $token = $prefix . "000000";
	return $token unless -e "$dir/$token";
	my $tries = 1000000;
	do {
		$token = $prefix . sprintf("%06d", rand(1000000));
	} while -e "$dir/$token" && --$tries;
	die "Failed to acquire unused file name $dir/$prefix*\n" unless $tries;
	return $token;
}

sub file_name_by_token ($$) {
	my $self = shift;
	my $token = shift;
	$token =~ s!/!%!g;
	return "$self->{dir}/$token";
}

sub delete_value ($$$) {
	my $self = shift;
	my ($key, $token) = @_;
	$token = $key if $token eq "";
	my $file_name = $self->file_name_by_token($token);
	return unless -e $file_name;
	unlink($file_name) or warn "Can't unlink $file_name: $!\n";
}

sub fetch_value ($$$) {
	my $self = shift;
	my ($key, $token) = @_;
	$token = $key if $token eq "";
	my $file_name = $self->file_name_by_token($token);
	my $value = eval { load_file($file_name); };
	warn $@ if $@;
	$self->decode_value(\$value);
	return $value;
}

sub store_value ($$$) {
	my $self = shift;
	my ($key, $token, $value) = @_;
	$token = $key
		if defined $token && $token eq "";
	$token = $key
		if !defined $token && !$self->{generic_filenames};
	$token = $self->generate_unique_token
		if !defined $token || $token eq "";
	my $file_name = $self->file_name_by_token($token);
	$self->encode_value(\$value);
	eval { save_file($file_name, \$value); };
	warn $@ if $@;
	$token = "" if $key eq $token;
	$token = undef if $@;
	return $token;
}

1;
