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

package Arch::Tree;

use Arch::Util qw(run_tla load_file _parse_revision_descs adjacent_revision);
use Arch::Name;
use Arch::Log;
use Arch::Inventory;
use Arch::Changes qw(:type);
use Arch::Changeset;

use Cwd;

sub new ($;$%) {
	my $class = shift;
	my $dir = shift || ".";
	die "No tree dir $dir\n" unless -d $dir;
	my ($root) = run_tla("tree-root", $dir);
	die "No tree root for dir $dir\n" unless $root;
	my %init = @_;

	my $self = {
		dir => $root,
		own_logs => $init{own_logs},
		hide_ids => $init{hide_ids},
	};

	return bless $self, $class;
}

sub root ($) {
	my $self = shift;

	return $self->{dir};
}

sub get_id_tagging_method ($) {
	my $self = shift;

	($self->{id_tagging_method}) = run_tla("id-tagging-method", "-d", $self->{dir})
		unless $self->{id_tagging_method};

	return $self->{id_tagging_method};
}

sub get_version ($) {
	my $self = shift;
	return $self->{version} if $self->{version};
	my ($version) = run_tla("tree-version", $self->{dir});
	return $self->{version} = $version;
}

sub set_version ($$) {
	my $self = shift;
	my $version = shift;

	delete $self->{version};
	run_tla("set-tree-version", "-d", $self->{dir}, $version);

	return $?;
}

sub get_log_versions ($) {
	my $self = shift;
	my @versions = run_tla("log-versions", "-d", $self->{dir});
	return wantarray? @versions: \@versions;
}

sub add_log_version ($$) {
	my $self = shift;
	my $version = shift;

	run_tla("add-log-version", "-d", $self->{dir}, $version);

	return $?;
}

sub get_log_revisions ($;$) {
	my $self = shift;
	my $version = shift || $self->get_version;
	$version =~ s!-(SOURCE|MIRROR)/!/!;
	my @revisions = run_tla("logs", "-f", "-d", $self->{dir}, $version);
	return wantarray? @revisions: \@revisions;
}

sub get_log ($$) {
	my $self = shift;
	my $revision = shift || die;

	my $message;
	if ($self->{own_logs}) {
		my $name = Arch::Name->new($revision);
		$name->is_valid('revision') or die "Invalid revision $revision\n";
		my @n = $name->get;
		my $version_subdir = $n[2] ne ""?
			"$n[1]--$n[2]/$n[1]--$n[2]--$n[3]": "$n[1]/$n[1]--$n[3]";
		my $subdir = "{arch}/$n[1]/$version_subdir/$n[0]/patch-log/$n[4]";
		my $file = "$self->{dir}/$subdir";
		$message = load_file($file) if -f $file;
	} else {
		$message = run_tla("cat-log", "-d", $self->{dir}, $revision);
	}
	return undef unless $message;
	return Arch::Log->new($message, hide_ids => $self->{hide_ids});
}

sub get_logs ($;$) {
	my $self = shift;
	my $version = shift || $self->get_version;
	my $versions = ref($version) eq 'ARRAY'? $version:
		$version eq '*'? $self->get_log_versions: [ $version ];

	my @logs = ();
	foreach (@$versions) {
		my $revisions = $self->get_log_revisions($_);
		foreach my $revision (@$revisions) {
			push @logs, $self->get_log($revision);
		}
	}
	return wantarray? @logs: \@logs;
}

sub get_log_revision_descs ($;$) {
	my $self = shift;
	my $version = shift;

	my $logs = $self->get_logs($version);
	my $revision_descs = [];
	foreach my $log (@$logs) {
		push @$revision_descs, $log->get_revision_desc;
	}
	return $revision_descs;
}

sub get_inventory ($) {
	my $self = shift;

	return Arch::Inventory->new($self->root);
}

# TODO: properly support escaping
sub get_changes ($) {
	my $self = shift;
	my @cha_temp = run_tla("changes", "-d", $self->{dir});

	my $changes = Arch::Changes->new;
	foreach my $line (@cha_temp) {
		next if ($line =~ /^\*/);

		$line =~ m!^([ADM=/-])([ />b-]) ([^\t]+)(?:\t([^\t]+))?$!
			or die("Unrecognized changes line: $line\n");

		my $type   = $1 ne '/' ? $1 : $2;
		my $is_dir = ($1 eq '/') || ($2 eq '/');
		my @args   = ($3, $4);
		$changes->add($type, $is_dir, @args);
	}

	return $changes;
}

sub get_changeset ($$) {
	my $self = shift;
	my $dir  = shift;

	die("Directory already exists: $dir\n")
		if (-d $dir);

	run_tla("changes", "-d", $self->{dir}, "-o", $dir);

	return -d $dir ? Arch::Changeset->new("changes.".$self->get_version(), $dir) : undef;
}

sub get_merged_log_text ($) {
	my $self = shift;
	my $text = run_tla("log-for-merge", "-d", $self->{dir});
	return $text;
}

sub get_merged_revision_summaries ($) {
	my $self = shift;
	my $text = $self->get_merged_log_text;
	my @hash = ();

	$text eq "" or $text =~ s/^Patches applied:\n\n//
		or die "Unexpected merged log output:\n$text\n";

	while ($text =~ s/^ \* (.*)\n(.+\n)*\n//) {
		push @hash, $1;
		my $summary = $2;
		$summary =~ s/^   //g;
		$summary =~ s/\n$//;
		push @hash, $summary;
	}
	die "Unexpected merged log sub-output:\n$text\n" if $text ne "";

	return @hash if wantarray;
	my %hash = @hash;
	return \%hash;
}

sub get_merged_revisions ($) {
	my $self = shift;

	my $revision_summaries = $self->get_merged_revision_summaries;
	my @array = sort keys %$revision_summaries;
	return wantarray ? @array : \@array;
}

sub get_missing_revisions ($;$) {
	my $self = shift;
	my $version = shift || $self->get_version;

	$self->{missing_revisions}->{$version} ||= [
		run_tla("missing", "-d", $self->{dir}, $version)
	];
	my $array = $self->{missing_revisions}->{$version};
	return wantarray ? @$array : $array;
}

sub get_missing_revision_descs ($;$) {
	my $self = shift;
	my $version = shift || $self->get_version;

	unless ($self->{missing_revision_descs}->{$version}) {
		my @revision_lines =
			map { /^\S/? (undef, $_): $_ }
			run_tla("missing -scD", "-d", $self->{dir}, $version);
		shift @revision_lines;  # throw away first undef

		my $revision_descs = _parse_revision_descs(4, \@revision_lines);
		$self->{missing_revision_descs}->{$version} = $revision_descs;
		$self->{missing_revisions}->{$version} =
			[ map { $_->{name} } @$revision_descs ];
	}
	return $self->{missing_revision_descs}->{$version};
}

*get_missing_revision_details = *get_missing_revision_descs;
*get_missing_revision_details = *get_missing_revision_details;

sub iterate_ancestry_logs ($;$$) {
	my $self = shift;
	my $cb = shift;
	my $no_continuation = shift || 0;

	my @collected = ();
	my $version = $self->get_version;
	my $revisions = $self->get_log_revisions($version);
	my $revision = $revisions->[-1];
	while ($revision) {
		my $log = $self->get_log($revision);

		# handle removed logs
		unless ($log) {
			$revision = adjacent_revision($revision, -1);
			next;
		}

		my $kind = $log->get_revision_kind;
		if ($kind eq 'import') {
			$revision = undef;
		} elsif (!$no_continuation && $kind eq 'tag') {
			$revision = $log->continuation_of;
		} else {
			$revision = adjacent_revision($revision, -1);
		}
		push @collected, $cb? $cb->($log): $log;
		last unless $log;  # undefined by callback
	}
	return \@collected;
}

sub get_ancestry_revision_descs ($;$$) {
	my $self = shift;
	my $filepath = shift;
	my $one_version = shift || 0;

	my ($is_dir, $changed, $version);
	if (defined $filepath) {
		my $full_filepath = "$self->{dir}/$filepath";
		# currently stat the existing tree file/dir
		$is_dir = -l $full_filepath? 0: -d _? 1: -f _? 0:
			die "No tree file or dir ($full_filepath)\n";
		$filepath =~ s!/{2,}!/!g;
		$filepath =~ s!^/|/$!!g;
		$filepath = "." if $filepath eq "";  # avoid invalid input die
	}

	return $self->iterate_ancestry_logs(sub {
		my $log = $_[0];
		if (defined $filepath) {
			$changed = $log->get_changes->is_changed("to", $filepath, $is_dir);
			return unless defined $changed;
		}
		my $revision_desc = $log->get_revision_desc;
		if ($one_version) {
			$version ||= $revision_desc->{version};
			if ($version ne $revision_desc->{version}) {
				$_[0] = undef;
				return;
			}
		}
		if (defined $filepath) {
			$revision_desc->{filepath} = $filepath;
			$revision_desc->{is_filepath_added}    = $changed->{&ADD}?    1: 0;
			$revision_desc->{is_filepath_renamed}  = $changed->{&RENAME}? 1: 0;
			$revision_desc->{is_filepath_modified} = $changed->{&MODIFY}? 1: 0;

			$filepath = $changed->{&RENAME}
				if $revision_desc->{is_filepath_renamed};
			$_[0] = undef
				if $revision_desc->{is_filepath_added};
		}
		return $revision_desc;
	});
}

sub add ($;@) {
	my $self = shift;
	my $opts = shift if ref($_[0]) eq 'HASH';
	my @files = @_;

	my @args = ();
	push @args, "--id", $opts->{id} if $opts->{id};
	push @args, @files;

	my $dir = $opts->{dir} || $self->{dir};

	my $cwd = getcwd();
	chdir($dir) && run_tla("add-id", @args);
	chdir($cwd);

	return $?;
}

sub delete ($;@) {
	my $self = shift;
	my $opts = shift if ref($_[0]) eq 'HASH';
	my @files = @_;

	my $dir = $opts->{dir} || $self->{dir};

	my $cwd = getcwd();
	chdir($dir) && run_tla("delete-id", @files);
	chdir($cwd);

	return $?;
}

sub move ($;@) {
	my $self = shift;
	my $opts = shift if ref($_[0]) eq 'HASH';
	my @files = @_;

	my $dir = $opts->{dir} || $self->{dir};

	my $cwd = getcwd();
	chdir($dir) && run_tla("move-id", @files);
	chdir($cwd);

	return $?;
}

sub make_log ($) {
	my $self = shift;

	my ($file) = run_tla("make-log", "-d", $self->{dir});
	return $file;
}

sub import ($;$@) {
	my $self = shift;
	return unless ref($self);  # ignore perl's import() method
	my $opts = shift if ref($_[0]) eq 'HASH';
	my $version = shift || $self->get_version;

	my @args = ();
	push @args, "--dir", $self->{dir} unless $opts->{dir};
	foreach my $opt (qw(archive dir log summary log-message)) {
		push @args, "--$opt", $opts->{$opt} if $opts->{$opt};
	}
	push @args, "--setup" if $opts->{setup};

	run_tla("import", @args, $version);

	return $?;
}

sub commit ($;$@) {
	my $self = shift;
	my $opts = shift if ref($_[0]) eq 'HASH';
	my $version = shift || $self->get_version;

	my @args = ();
	push @args, "--dir", $self->{dir} unless $opts->{dir};
	foreach my $opt (qw(archive dir log summary log-message file-list)) {
		push @args, "--$opt", $opts->{$opt} if $opts->{$opt};
	}
	foreach my $opt (qw(strict seal fix out-of-date-ok)) {
		push @args, "--$opt" if $opts->{$opt};
	}

	run_tla("commit", @args, $version);

	return $?;
}

1;

__END__

=head1 NAME

Arch::Tree - class representing Arch tree

=head1 SYNOPSIS

    use Arch::Tree;
    my $tree = Arch::Tree->new;  # assume the current dir

    print map { "$_\n" } $tree->get_log_versions;

    foreach my $log ($tree->get_logs) {
        print "-" x 80, "\n";
        print $log->standard_date, "\n";
        print $log->summary, "\n\n";
        print $log->body;
    }

=head1 DESCRIPTION

This class represents the working tree concept in Arch and provides some
useful methods.

=head1 METHODS

The following methods are available:

B<new>,
B<root>,
B<get_version>,
B<set_version>,
B<get_log_versions>,
B<add_log_version>,
B<get_log_revisions>,
B<get_log>,
B<get_logs>,
B<get_log_revision_descs>,
B<get_inventory>,
B<get_changes>,
B<get_changeset>,
B<get_merged_log_text>,
B<get_merged_revision_summaries>,
B<get_merged_revisions>,
B<get_missing_revisions>,
B<get_missing_revision_descs>,
B<iterate_ancestry_logs>,
B<get_ancestry_revision_descs>,
B<add>,
B<import>,
B<commit>.

=over 4

=item B<new> [I<dir-name>]

Construct the Arch::Tree object associated with the existing directory
I<dir-name>. The default is the current '.' directory.

=item B<root>

Returns the project tree root.

=item B<get_version>

Returns the fully qualified tree version.

=item B<set_version> I<version>

Changes the tree version to I<version>.

=item B<get_log_versions>

Returns all version names (including the main one and merged ones) for which
logs are stored in the tree. In the scalar context returns arrayref.

=item B<add_log_version> I<version>

Add log version I<version> to project tree.

=item B<get_log_revisions> [I<version>]

Returns all revision names of the given I<version> (the default is the tree
version) for which logs are stored in the tree. In the scalar context
returns arrayref.

=item B<get_log> I<revision>

Return Arch::Log object corresponding to the tree log of the given I<revision>.

=item B<get_logs> [I<version>]

Return Arch::Log objects corresponding to the tree logs of the given I<version>.
In the scalar context returns arrayref.

The default I<version> is the tree version (see C<get_version>).
A special version name '*' may be used, in this case all logs in
C<get_log_versions> are returned. I<version> may be arrayref as well
with the similar results.

=item B<get_log_revision_descs> [I<version>]

Returns arrayref of log revision description hashes corresponding to
I<version>. The optional I<version> argument may get the same values that
are supported by B<get_logs>.

=item B<get_inventory>

Returns L<Arch::Inventory> object for the project tree.

=item B<get_changes>

Returns a list of uncommited changes in the project tree.

=item B<get_changeset> I<dir>

Creates an B<Arch::Changeset> of the uncommited changes in the
tree. The directory I<dir> is used to store the changeset and must not
already exist. It will not be automatically removed.

=item B<get_merged_log_text>

This is just the output of "tla log-for-merge".

=item B<get_merged_revision_summaries>

Returns hash (actually sorted array of pairs) or hashref in the scalar
context. The pair is for every merged revision: full-name => summary.

=item B<get_merged_revisions>

The list of all merged in (present in the changes) full revisions.
In the scalar context returns arrayref.

=item B<get_missing_revisions> [I<version>]

The list of all missing revisions corresponding to I<version>.
In the scalar context returns arrayref.

The default I<version> is the tree version (see C<get_version>).

=item B<get_missing_revision_descs> [I<version>]

The hashref of all missing revision descriptions corresponding to I<version>.
The hash keys are revisions and the values are hashrefs with keys
I<name>, I<summary>, I<creator>, I<email>, I<date>, I<kind>.

The default I<version> is the tree version (see C<get_version>).

=item B<iterate_ancestry_logs> I<callback> [I<no_continuation>]

For each ancestry revision (calculated from tree logs), call I<callback>
that receives the B<Arch::Log> object and should return some list content.
The values returned by the I<callback> are collected in one array and are
returned as arrayref.

If I<no_continuation> is set, then do not follow tags backward.

=item B<get_ancestry_revision_descs> [I<filepath>] [I<one_version>]

The arrayref of all ancestry revision descriptions in the backward order.
If I<filepath> is given, then only revisions that modified the given file
(or dir) are returned. The revision description is hashref with keys
I<name>, I<summary>, I<creator>, I<email>, I<date>, I<kind>, I<filepath>.

If I<one_version> is set then stop to report revision descriptions from the
versions different than the initial version. I<one_version> flag is similar
to I<no_continuation> flag in another method, but not the same, since it is
possible to tag into the same version.

=item B<add> [{ I<options> }] file ...

Similar to 'tla add'.

=item B<import> [{ I<options> }] [I<version>]

Similar to 'tla import'.

=item B<commit> [{ I<options> }] [I<version>]

Commit changes in tree.

=back

=head1 BUGS

Awaiting for your reports.

=head1 AUTHORS

Mikhael Goikhman (migo@homemail.com--Perl-GPL/arch-perl--devel).

Enno Cramer (uebergeek@web.de--2003/arch-perl--devel).

=head1 SEE ALSO

For more information, see L<tla>, L<Arch::Log>, L<Arch::Inventory>,
L<Arch::Changes>, L<Arch::Util>, L<Arch::Name>.

=cut
