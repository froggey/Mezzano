#!/usr/bin/perl
# $Header: /usr/local/cvsrep/cl-ppcre/test/perltest.pl,v 1.1 2008/07/06 21:24:39 edi Exp $

# This is a heavily modified version of the file 'perltest' which
# comes with the PCRE library package, which is open source software,
# written by Philip Hazel, and copyright by the University of
# Cambridge, England.

# The PCRE library package is available from
# <ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/>

sub string_for_lisp {
  my(@a, $t, $in_string, $switch);

  my $string = shift;
  $string =~ s/\\/\\\\/g;
  $string =~ s/"/\\"/g;

  return "\"$string\""
    if $string =~ /^[\n\x20-\x7f]*$/;

  $in_string = 1;
  foreach $c (split(//, $string)) {
    if (ord $c >= 32 and ord $c < 127) {
      if ($in_string) {
        $t .= $c;
      } else {
        $in_string = 1;
        $t = $c;
      }
    } else {
      if ($in_string) {
        push @a, "\"$t\"";
        $in_string = 0;
        $switch = 1;
      }
      push @a, ord $c;
    }
  }
  if ($switch) {
    if ($in_string) {
      push @a, "\"$t\"";
    }
    '(' . (join ' ', @a) . ')';
  } else {
    "\"$t\"";
  }
}

NEXT_RE: while (1) {
  last
    if !($_ = <>);
  next
    if $_ eq "";

  $pattern = $_;

  while ($pattern !~ /^\s*(.).*\1/s) {
    last
      if !($_ = <>);
    $pattern .= $_;
  }

  chomp($pattern);
  $pattern =~ s/\s+$//;
  $pattern =~ s/\+(?=[a-z]*$)//;

  $multi_line_mode = ($pattern =~ /m[a-z]*$/) ? 't' : 'nil';
  $single_line_mode = ($pattern =~ /s[a-z]*$/) ? 't' : 'nil';
  $extended_mode = ($pattern =~ /x[a-z]*$/) ? 't' : 'nil';
  $case_insensitive_mode = ($pattern =~ /i[a-z]*$/) ? 't' : 'nil';
  $pattern =~ s/^(.*)g([a-z]*)$/\1\2/;

  $pattern_for_lisp = $pattern;
  $pattern_for_lisp =~ s/[a-z]*$//;
  $pattern_for_lisp =~ s/^\s*(.)(.*)\1/$2/s;
  $pattern_for_lisp =~ s/\\/\\\\/g;
  $pattern_for_lisp =~ s/"/\\"/g;

  $pattern = "/(?#)/$2"
    if ($pattern =~ /^(.)\1(.*)$/);

  while (1) {
    last NEXT_RE
      if !($_ = <>);

    chomp;

    s/\s+$//;
    s/^\s+//;

    last
      if ($_ eq "");

    $info_string = string_for_lisp "\"$_\" =~ $pattern";
    $x = eval "\"$_\"";

    @subs = ();

    eval <<"END";
if (\$x =~ ${pattern}) {
  push \@subs,\$&;
  push \@subs,\$1;
  push \@subs,\$2;
  push \@subs,\$3;
  push \@subs,\$4;
  push \@subs,\$5;
  push \@subs,\$6;
  push \@subs,\$7;
  push \@subs,\$8;
  push \@subs,\$9;
  push \@subs,\$10;
  push \@subs,\$11;
  push \@subs,\$12;
  push \@subs,\$13;
  push \@subs,\$14;
  push \@subs,\$15;
  push \@subs,\$16;
}

\$test = sub {
  my \$times = shift;

  my \$start = time;
  for (my \$i = 0; \$i < \$times; \$i++) {
    \$x =~ ${pattern};
  }
  return time - \$start;
};
END

    $counter++;
    print STDERR "$counter\n";

    if ($@) {
      $error = 't';
    } else {
      $error = 'nil';
    }

    print "($counter $info_string \"$pattern_for_lisp\" $case_insensitive_mode $multi_line_mode $single_line_mode $extended_mode " . string_for_lisp($x) . " $error ";
    if (!@subs) {
      print 'nil nil';
    } else {
      print string_for_lisp($subs[0]) . ' (';
      undef $not_first;
      for ($i = 1; $i <= 16; $i++) {
        print ' '
          unless $i == 1;
        if (defined $subs[$i]) {
          print string_for_lisp $subs[$i];
        } else {
          print 'nil';
        }
      }
      print ')';
    }
    print ")\n";
  }
}
