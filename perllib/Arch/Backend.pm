# Arch Perl library, Copyright (C) 2005 Mikhael Goikhman
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

package Arch::Backend;

use Arch::Util;

use Exporter;
use vars qw(@ISA @EXPORT_OK $EXE $NAME $VRSN);

@ISA = qw(Exporter);
@EXPORT_OK = qw(
	arch_backend arch_backend_name arch_backend_version is_baz is_tla
	has_archive_setup_cmd has_file_diffs_cmd has_register_archive_name_arg
	has_tree_version_dir_opt has_tree_id_cmd has_set_tree_version_cmd
);

BEGIN {
	$EXE ||= $ENV{ARCH_BACKEND} || $ENV{TLA} || $ENV{BAZ} || "tla";
}

sub arch_backend (;$) {
	$EXE = shift if $_[0];
	return $EXE;
}

sub _parse_name_version () {
	my ($line1) = Arch::Util::run_tla("--version") || "";
	if ($line1 =~ /\b(tla|baz)(?:--.*--)?.*(\d+\.\d+(?:.\d+)?)/) {
		($NAME, $VRSN) = ($1, $2);
	} else {
		($NAME, $VRSN) = ("tla", "1.3");
		if ($EXE =~ /(tla|baz)(?:-(\d\.\d+(?:.\d+)?))/) {
			$NAME = $1;
			$VRSN = $2 if $2;
		}
		warn "Can't parse '$EXE --version' and determine arch backend name/version.\n"
			. "Please check \$ARCH_BACKEND and optionally notify arch-perl developers.\n"
			. "Assuming ($NAME, $VRSN). Set \$ARCH_PERL_QUIET to disable this warning.\n"
			unless $ENV{ARCH_PERL_QUIET};
	}
	#print "arch_backend name and version: ($NAME, $VRSN)\n";
	return ($NAME, $VRSN);
}

sub arch_backend_name () {
	return $NAME ||= (_parse_name_version)[0];
}

sub arch_backend_version () {
	return $VRSN ||= (_parse_name_version)[1];
}

sub is_baz () {
	return arch_backend_name() eq "baz";
}

sub is_tla () {
	return arch_backend_name() eq "tla";
}

sub has_archive_setup_cmd () {
	return is_tla();
}

sub has_file_diffs_cmd () {
	return is_tla() && arch_backend_version() =~ /^1\.[12]/;
}

sub has_register_archive_name_arg () {
	return is_tla();
}

sub has_tree_version_dir_opt () {
	return is_baz();
}

sub has_tree_id_cmd () {
	return is_baz();
}

sub has_set_tree_version_cmd () {
	return is_tla();
}

1;

__END__

=head1 NAME

Arch::Backend - Arch backend feature specific functions

=head1 SYNOPSIS

    use Arch::Backend qw(arch_backend is_baz has_file_diffs_cmd);

    my $exe = arch_backend();  # or $Arch::Backend::EXE
    print "No tree, try '$exe init-tree'\n";

    print "Using baz ", Arch::Backend::arch_backend_version,
		" as a backend\n" if is_baz();

    my $cmd = has_file_diffs_cmd()
        ? "file-diffs"
        : "file-diff";
    Arch::Util::run_tla($cmd, $filename);

=head1 DESCRIPTION

A set of helper functions suitable for GNU Arch related projects in Perl.

Higher (object oriented) levels of Arch/Perl library make use of these
helper functions to query certain aspects (like incompatible features)
of the actual arch backend used.

=head1 FUNCTIONS

The following functions are available:

B<arch_backend>,
B<arch_backend_name>,
B<arch_backend_version>,
B<is_baz>,
B<is_tla>,
B<has_archive_setup_cmd>,
B<has_file_diffs_cmd>,
B<has_register_archive_name_arg>,
B<has_tree_version_dir_opt>,
B<has_tree_id_cmd>,
B<has_set_tree_version_cmd>.

=over 4

=item B<arch_backend> [I<exe>]

Return or set the arch backend executable, like "/opt/bin/tla" or "baz-1.3".
By default, "tla" is used.

=item B<arch_backend_name>

Return the brand name of the arch backend, "tla" or "baz".

=item B<arch_backend_version>

Return the arch backend version, like "1.3.1".

=item B<is_tla>

Return true if B<arch_backend_name> is "tla".

=item B<is_baz>

Return true if B<arch_backend_name> is "baz".

=item B<has_archive_setup_cmd>

Return true if the arch backend has "archive-setup" command. baz removed
this command.

=item B<has_file_diffs_cmd>

Return true if the arch backend has "file-diffs" command. It was renamed
to "file-diff" in tla-1.3.

=item B<has_register_archive_name_arg>

Return true if the arch backend's "register-archive" command supports two
positional arguments, one of which is archive name. baz-1.3 removed such
syntax; the previous baz versions supported this syntax, but it was
useless, since the archive was accessed anyway.

=item B<has_tree_version_dir_opt>

Return true if the arch backend's "tree-version" command supports "-d"
options. This is true for baz.

=item B<has_tree_id_cmd>

Return true if the arch backend has "tree-id" command.
This is true for baz.

=item B<has_set_tree_version_cmd>

Return true if the arch backend has "set-tree-version" command.
baz removed this command and merged it into "tree-version".

=back

=head1 BUGS

This module uses heuristics and does not (intend to) provide the perfect
information. Requires constant updating.

=head1 AUTHORS

Mikhael Goikhman (migo@homemail.com--Perl-GPL/arch-perl--devel).

=head1 SEE ALSO

For more information, see L<tla>, L<baz>, L<Arch>.

=cut
