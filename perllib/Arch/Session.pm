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

package Arch::Session;

use base 'Arch::Storage';

use Arch::Util qw(run_tla _parse_revision_descs);
use Arch::TempFiles qw(temp_dir_name);
use Arch::Changeset;
use Arch::Log;
use Arch::Tree;

sub new ($%) {
	my $class = shift;
	my %init = @_;
	my $self = $class->SUPER::new(%init);
	$self->clear_cache;
	return $self;
}

sub archives ($) { 
	my $self = shift;
	$self->{archives} ||= [ run_tla("archives -n") ];
	return $self->{archives};
}
 
*is_archive_registered = *Arch::Storage::is_archive_managed;
*is_archive_registered = *is_archive_registered;

sub categories ($;$) {
	my $self = shift;
	my $archive = $self->_name_operand(shift, 'archive');

	unless ($self->{categories}->{$archive}) {
		$self->{categories}->{$archive} = [ run_tla("categories", $archive) ];
	}
	return $self->{categories}->{$archive};
}

sub branches ($;$) {
	my $self = shift;
	my $category = $self->_name_operand(shift, 'category');

	unless ($self->{branches}->{$category}) {
		$self->{branches}->{$category} = [ run_tla("branches", $category) ];
	}
	return $self->{branches}->{$category};
}

sub versions ($;$) {
	my $self = shift;
	my $branch = $self->_name_operand(shift, 'branch');

	unless ($self->{versions}->{$branch}) {
		$self->{versions}->{$branch} = [ run_tla("versions", $branch) ];
		# temporarily do this for backward compatibility
		$self->{versions}->{$branch} = [ map { s/--/----/; $_ } grep !/--.*--/, @{$self->{versions}->{$branch}} ]
			if $branch->branch eq '';
	}
	return $self->{versions}->{$branch};
}

sub revisions ($;$) {
	my $self = shift;
	my $version = $self->_name_operand(shift, 'version');

	unless ($self->{revisions}->{$version}) {
		$self->{revisions}->{$version} = [ run_tla("revisions", $version) ];
	}
	return $self->{revisions}->{$version};
}

sub get_revision_descs ($;$) {
	my $self = shift;
	my $version = $self->_name_operand(shift, 'version');

	unless ($self->{revision_descs}->{$version}) {
		my $nonarch_version = $version->nan;

		# $ok is used to work around the tla bug with branchless version
		# $prev_line is used to track revisions with no (empty) summary
		my $ok = 0;
		my $prev_line = "";

		my @revision_lines = map { s/^        //? $_: undef }
			grep {
				$ok = /^      \Q$nonarch_version\E$/ if /^      [^ ]/;
				my $end = ($prev_line =~ /^        /) && ($_ eq "");
				$prev_line = $_;
				($end || /^        /) && $ok
			}
			run_tla("abrowse --desc", $version);

		my $revision_descs = _parse_revision_descs(2, \@revision_lines);
		$self->{revision_descs}->{$version} = $revision_descs;
		$self->{revisions}->{$version} = [ map { $_->{name} } @$revision_descs ];
	}
	return $self->{revision_descs}->{$version};
}

*revision_details = *get_revision_descs; *revision_details = *revision_details;

sub clear_cache ($;@) {
	my $self = shift;
	my @keys = @_;

	@keys = qw(archives categories branches versions revisions revision_descs);
	foreach (@keys) {
		if (@_ && !exist $self->{$_}) {
			warn __PACKAGE__ . "::clear_cache: unknown key ($_), ignoring\n";
			next;
		}
		$self->{$_} = $_ eq 'archives'? undef: {};
	}

	return $self;
}

# [
#   [ category1, [
#     [ branch1, [
#       [ version1, start_revision1, end_revision1 ],
#       [ version2, start_revision2, end_revision2 ],
#     ] ],
#     [ branch2, [
#       [ version3, start_revision3, end_revision3 ],
#       [ version4, start_revision4, end_revision4 ],
#     ] ],
#     ...,
#   ] ],
# ]

sub expanded_archive_info ($;$$) {
	my $self = shift;
	my $archive_plus = $self->_name_operand(shift);
	my $full_listing = shift || 0;  # currently ignored

	my $infos = [];
	my @category_infos = split(/^\b/m, join('',
		map { s/^  //; "$_\n" } grep { /^  / }
			run_tla("abrowse $archive_plus")
	));

	my $error = 0;
	CATEGORY_ITEM:
	foreach (@category_infos) {
		my ($category, $branch_infos) = /^([^\s]+)\n(  .*)$/s;
		push @$infos, [ $category, [] ];
		unless (defined $category) {
			$error = 1; next CATEGORY_ITEM;
		}

		my @branch_infos = split(/^\b/m, join('',
			map { s/^  // or $error = 1; "$_\n" }
				split("\n", $branch_infos)
		));
		$error = 1 unless @branch_infos;
		foreach (@branch_infos) {
			my ($branch, $version_infos) = /^\Q$category\E(?:--([^\s]+))?\n(  .*)$/s;
			$branch = "" if defined $version_infos && !defined $branch;
			unless (defined $branch) {
				$error = 1; next CATEGORY_ITEM;
			}
			push @{$infos->[-1]->[1]}, [ $branch, [] ];

			my @version_infos = split(/^\b/m, join('',
				map { s/^  // or $error = 1; "$_\n" }
					split("\n", $version_infos)
			));
			$error = 1 unless @version_infos;
			foreach (@version_infos) {
				my ($version, $revision0, $revisionl) = /^\Q$category\E(?:--)?\Q$branch\E--([^\s]+)(?:\n  ([^\s]+)(?: \.\. ([^\s]+))?\n)?$/s;
				unless (defined $version) {
					$error = 1; next CATEGORY_ITEM;
				}
				my $revisions2 = [];
				if ($full_listing) {
					push @$revisions2, $revision0 if defined $revision0;
					push @$revisions2, $revisionl if defined $revisionl;
				} else {
					$revision0 = '' unless defined $revision0;
					$revisionl = '' unless defined $revisionl;
					push @$revisions2, $revision0, $revisionl;
				}
				push @{$infos->[-1]->[1]->[-1]->[1]}, [ $version, @$revisions2 ];
			}
		}
	} continue {
		if ($error) {
			warn "Unexpected abrowse output, skipping:\n$_\n";
			pop @$infos;
			$error = 0;
		}
	}
	return $infos;
}

sub get_revision_changeset ($$;$) {
	my $self = shift;
	my $revision = shift;
	my $dir = defined $_[0]? shift: temp_dir_name("arch-changeset");
	die "get_changeset: incorrect dir ($dir)\n" unless $dir && !-d $dir;

	run_tla("get-changeset", $revision, $dir);
	return Arch::Changeset->new($revision, $dir);
}

sub get_changeset ($;$) {
	my $self = shift;
	my $dir = shift;
	my $revision = $self->working_name;
	die "get_tree: no working revision\n" unless $revision->is_valid('revision');
	return $self->get_revision_changeset($revision, $dir);
}

sub get_revision_log ($$) {
	my $self = shift;
	my $revision = shift || die "get_revision_log: No revision given\n";
	my $message = run_tla("cat-archive-log", $revision);
	die "Can't get log of $revision from archive.\n"
		. "Unexisting revision or system problems.\n"
		unless $message;
	return Arch::Log->new($message);
}

sub get_log ($) {
	my $self = shift;
	my $revision = $self->working_name;
	die "get_tree: no working revision\n" unless $revision->is_valid('revision');
	return $self->get_revision_log($revision);
}

sub get_tree ($;$$) {
	my $self = shift;
	my $revision = $self->_name_operand(shift);
	die "get_tree: no b|v|r\n" unless $revision->is_valid('branch+');

	my $dir = shift || temp_dir_name("arch-tree");
	die "get_tree: no directory name (internal error?)\n" unless $dir;
	die "get_tree: directory already exists ($dir)\n" if -d $dir;

	run_tla("get --silent --no-pristine", $revision, $dir);
	die "Can't get revision $revision from archive.\n"
		. "Unexisting revision or system problems.\n"
		unless -d $dir;
	return Arch::Tree->new($dir);
}

sub init_tree ($$;$) {
	my $self = shift;
	my $version = $self->_name_operand(shift, "version");
	my $dir = shift || ".";

	run_tla("init-tree", "-d", $dir, $version);
	return undef unless $? == 0;
	return Arch::Tree->new($dir);
}

sub my_id ($;$) {
	my $self = shift;
	my $userid = shift;

	if (defined $userid) {
		return 0 unless $userid =~ /<.+\@.*>/;
		run_tla("my-id", $userid);
		return !$?;
	} else {
		($userid) = run_tla("my-id");
		return $userid;
	}
}

1;

__END__

=head1 NAME

Arch::Session - access arch archives

=head1 SYNOPSIS

    use Arch::Session;

    my $session = Arch::Session->new;

    my $rev  = 'migo@homemail.com--Perl-GPL/arch-perl--devel--0--patch-1';
    my $log  = $session->get_revision_log($rev);
    my $cset = $session->get_revision_changeset($rev);
    my $tree = $session->get_tree($rev);

=head1 DESCRIPTION

Arch::Session provides an interface to access changesets and logs
stored in arch archives.

=head1 METHODS

The following common methods (inherited and pure virtual that
this class implements) are documented in L<Arch::Storage>:

B<new>,
B<init>,
B<working_name>,
B<working_names>,
B<fixup_name_alias>,
B<is_archive_managed>,
B<expanded_revisions>.

B<archives>,
B<categories>,
B<branches>,
B<versions>,
B<revisions>,
B<get_revision_descs>,
B<expanded_archive_info>,
B<get_revision_changeset>,
B<get_changeset>,
B<get_revision_log>,
B<get_log>.

Additionally, the following methods are available:

B<clear_cache>,
B<get_tree>,
B<init_tree>,
B<my_id>.

=over 4

=item B<clear_cache> [key ..]

For performance reasons, most method results are cached (memoized in fact).
Use this method to explicitly request this cache to be cleared.

By default all cached keys are cleared; I<key> may be one of the strings
'archives', 'categories', 'branches', 'versions', 'revisions' or
'revision_descs'.

=item B<get_tree> [I<revision> [I<dir>]]

Construct a working tree for I<revision> or B<working_name> in
I<dir>. If I<dir> is not specified, a new temporary directory is
automatically created.

=item B<init_tree> I<dir>

Run C<tla init-tree> in I<dir>.

=item B<my_id> [I<newid>]

Get or set C<tla my-id>.

=back

=head1 BUGS

No known bugs.

=head1 AUTHORS

Mikhael Goikhman (migo@homemail.com--Perl-GPL/arch-perl--devel).

Enno Cramer (uebergeek@web.de--2003/arch-perl--devel).

=head1 SEE ALSO

For more information, see L<tla>, L<Arch::Storage>, L<Arch::Library>,
L<Arch::Name>, L<Arch::Log>, L<Arch::Changeset>.

=cut