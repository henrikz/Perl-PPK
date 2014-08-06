package PPK;

use strict;
use Carp;
use Ted::Lambda qw( ncurry );

require Exporter;
our @ISA     = ('Exporter');
our @EXPORT = qw(do_applicative pure zero char re
                 seq predicate choice many many1 sepby sepby1 endby1
                 chainr1 chainl1 bracket first1 second1 last1);

=head1 NAME

PPK - Perfect Parser Kit

=head1 SYNOPSIS

use PPK;

=head1 DESCRIPTION

Combinatory parser library, that lets you build complicated parsers
by combining basic parsers in a number of ways.

Your parser will work without the extra compilation step that is necessary
when using a parser generator such as Yapp.

=head1 FUNCTIONS

=over 4

=cut

=item do_applicative($star, $pure, $f, @values)

Does an application of a series of applicative functors

  $pure    :: a -> F a
  $combine :: F (a -> b) -> F a -> F b
  $f       :: (a,b ...) -> z
  $values  :: (a,b ...)

f is then transformed into
  $cf      :: F (a -> b -> ... -> z)

Example: The list [] functor

We need to supply the appropiate pure and combine functions.

list_pure is a function that takes a value, and puts in into the functor, ie. the list:
  sub list_pure {
      my ($a) = @_;
      return [ $a ];
  }

list_combine take a list of functions, and a list of values and combines the two:
  sub list_combine {
      my ($ff, $fv) = @_;
      my @ret = ();
      my ($f, $v);
      for $f(@$ff) {
          for $v(@$fv) {
              push @ret, $f->($v);
          }
      }
      return \@ret;
  }

Note, if $ff contains only one function, list_compine works like a simple map.

Now, you can do eg.

  do_applicative(\&list_pure,
                 \&list_combine,
                 sub { ($a,$b) = @_; "$a,$b" },
                 [1..2],
                 ['a'..'b']);

     -> ['1,a', '1,b','2,a', '2,b']

=cut
sub do_applicative {
    my $pure     = shift or croak 'No pure';
    my $combine  = shift or croak 'No combine';
    my $f        = shift or croak 'No f';

    my $cf = $pure->(ncurry($f, scalar @_)); # TODO: curry   -> partial
                                             #       ncurry  -> curry
    my $v;
    while (defined ($v = shift @_)) {
        $cf = $combine->($cf,$v);
    }
    return $cf;
}


=item fmap($f, $p)

Maps parser value obtained from the parser $p by applying $f.

=cut
sub fmap {
    my $f = shift or croak 'No f';
    my $p = shift or croak 'No p';
    return combine(pure($f), $p);
}

=item pure($v)

Pure parser will always succeed without consuming anything.
$v will be the parser value returned from the parser.

### $v -> ($s -> [$v, $s])

=cut
sub pure {
    my $v = shift; croak if !defined $v;
    return sub {
        my $inp = shift;
        return [$v, $inp];
    }
}

# applicative parser combination
# (<*>) :: Parser (a -> a') -> Parser a -> Parser a'
# ff <*> fv = \inp -> do (f', inp')  <- ff inp
#                        (v', inp'') <- fv inp'
#                        return (f' v', inp'')
sub combine {
    my $ff = shift or croak 'No ff';
    my $fv = shift or croak 'No fv';
    
    sub {
        my $inp = shift;

        my $fs = $ff->($inp);
        if (ref $fs eq 'ARRAY') {
            my ($fm, $inpm) = @$fs;

            my $vs = $fv->($inpm);
            # Possible lazyness
            if ( ref $vs eq 'CODE') {
                $vs = $vs->($inpm);
            }
            if (ref $vs eq 'ARRAY') {
                my ($vm, $inpmm) = @$vs;
                return [$fm->($vm), $inpmm];
            }
            else {
                return $vs;
            }
        } else {
            return $fs;
        }
    };
}

## (>>-) :: Parser a -> (a -> Parser b) -> Parser b
## (>>=) :: (a -> (a,s)) -> (a -> (b -> (b,s))) -> (b -> (b,s))
## mv: a -> (a,s)
## mf: (a -> (b -> (b,s)))
sub bindp {
    my $mv = shift or croak 'No mv';
    my $mf = shift or croak 'No mf';

    sub {
        my $inp = shift;
        my $res = $mv->($inp);
        ## Lazy?
        if ( ref $res eq 'CODE' ) {
            $res = $res->($inp);
        }
        if ( ref $res eq 'ARRAY' ) {
            my ($v, $inpm) = @$res;
            return $mf->($v)->($inpm);
        }
        else {
            return $res;
        }
    };
}

### Make a lazy parser
### Example
###    strict: many($p)
##
###    lazy  : recur(\&many, $p)
### This is necessary when constructing recursive parsers
sub recur {
    my $f    = shift;# or croak 'No f';
    my $args = \@_;
    my $p    = undef;

    sub {
        # Optimization, don't know if it works
        $p = $f->(@$args) if ! defined $p;
        return $p->(@_);
    };
}


### TODO: End of file, and not end of file
sub string2errmsg {
    my $inp    = shift;
    my $maxlen = shift || 25;
    
    if ( length($inp) == 0) {
        $inp = "<end-of-string>";
    }
    elsif ( length($inp) >= $maxlen) {
        $inp = substr($inp, 0, $maxlen) . " ...";
    }
    return $inp;
}


=item zero($expected)

Parser that allways fails.

$expected will be parsed on to the error
value returned by the application of the parser.

=cut
sub zero {
    my $expected   = shift;
    
    sub {
        my $inp      = shift;

        return { type       => 'Parse error',
                 input      => $inp,
                 expected   => $expected,
             };
    }
};

=item char($c)

Parses character $c. If the first character found it not equal to $c, this
parser fails.

=cut
sub char {
    my $c = shift; croak 'No c' if ! defined $c;
    
    sub {
        my $inp = shift;
        my ($s1, $s2) = (substr($inp, 0, 1), substr($inp, 1));
        if ($s1 eq $c) {
            return pure($s1)->($s2);
        }
        else {
            return zero($c)->($inp);
        }
    }
}

=item re([$f, ]$regex[, $description)

Regex parser. Will parse if the beginning of the string matches $regex.

You can optionally supply:
  $f     map function to be applied to the parsed value
  $desc  description to be used for error reporting, eg. 'number' or 'float' or 'string'

=cut
sub re {
    my $f;
    my $s           = shift or croak 'No s';
    if ( ref $s eq 'CODE') {
        $f = $s;
        $s = shift or croak 'No s';
    }
    my $description = shift || $s;
    
    my $regex = qr{^($s)(.*)$};
    
    sub {
        my $inp = shift;
        my ($s1,$s2) = $inp =~ m{$regex};
        if (defined $1) {
            $s1 = $f->($s1) if defined $f;
            return pure($s1)->($s2);
        }
        else {
            return zero($description)->($inp);
        }
    };
}

=item seq($f, $p1, $p2 ...)

The resulting parser will try to apply the parsers $p1, $p2 ... in sequence. If
one of the parsers fails, seq will fails with that parsers error record.
Combines the parsed values into a list.


Sample:
  seq(char('a'), char('b'))

This parser will parse 'ab', yielding $f->('a','b') as it's parsed value.

=cut
sub seq {
    my $f   = shift or croak 'No f';
    return do_applicative(\&pure, \&combine, $f, @_);
}


=item predicate($pred, p)

If parser $p succeeds, then test the result with $pred and subsequently
succeed only if $pred->(<result of $p>) returns a true value.

=cut
sub predicate {
    my $pred = shift or croak 'No pred';
    my $p    = shift or croak 'No p';

    return bindp($p, sub {
                     my $v = shift;
                     if ($pred->($v)) {
                         return pure($v);
                     }
                     else {
                         return zero("Parsed value [$v] failed predicate");
                     }
                 });
}

=item choice($p1, $p2 ...)

Try a set of parsers. Return the result of the first succesful one.

=cut
sub choice {
    my @parsers = @_;
    sub {
        my $inp = shift;
        my @results = map {
            my $st = $_->($inp);
            ## Possible lazyness
            if (ref $st eq 'CODE') {
                $st = $st->($inp);
            }
            if (ref $st eq 'ARRAY') {
                return $st;
            }
            $st;
        } @parsers;
        ## Select the errors where the parse advanced the most,
        ## ie. input left is as small as possible
        my ($errors) = mostn(sub { return -(length(shift()->{input})); },
                             \@results);
        my @expected = map { $_->{expected} } @$errors;
        return zero(flatten(\@expected))->($errors->[0]->{input});
    }
}


## Ideas for support functions: list, last, number, etc.
sub listseq {
    return seq(\&list, @_);
}

sub cons2 {
    my ($p1, $p2) = @_;
    return seq(\&cons, $p1, $p2);
}

=item many($p)

Apply $p none or many times. Resulting value is a list of the parsed values, eg.

 many(char('a')) applied to 'aaa' will give you ['a', 'a', 'a'].
 many(char('a')) applied to 'baa' will give you [].

=cut
sub many {
    my $p  = shift or croak 'No p';
    
    return sub {
        my $inp = shift;
        my ($item, $token);
        my @ret = ();
        
        while (ref($token = $p->($inp)) eq 'ARRAY') {
            ($item, $inp) = @$token;
            push @ret, $item;
        }
        return [\@ret, $inp];
    };
}

=item many1($p)

Like many, but requires at least one succesfull application of $p, otherwise it will fail.

=cut
sub many1 {
    my $p = shift or croak 'No p';

    return cons2($p, many($p));
}


### Sequence delimited by $s. Delimiters are ignored in the parsing result
sub sepby1 {
    my $p = shift or croak 'No p';
    my $s = shift or croak 'No s';

    return cons2($p, many(last1($s, $p)));
}

sub endby1 {
    my $p = shift or croak 'No p';
    my $s = shift or croak 'No s';

    return many1(first1($p, $s));
}

### Left associative operators
sub chainl1 {
    my $p = shift or croak 'No p';
    my $s = shift or croak 'No s';

    return seq(\&map2ltree, $p, many(listseq($s, $p)));
}

## Right associative operators
sub chainr1 {
    my $p  = shift or croak 'No p';
    my $s  = shift or croak 'No s';

    return seq(\&consrtree,
               $p,
               choice(listseq($s, recur(\&chainr1, $p, $s)),
                      pure([])));
}

sub bracket {
    my $open  = shift or croak 'No open';
    my $p     = shift or croak 'No p';
    my $close = shift or croak 'No close';

    return seq(\&rtsecond, $open, $p, $close);
}

sub first1 {
    return seq(\&rtfirst, @_);
}

sub second1 {
    return seq(\&rtsecond, @_);
}

sub last1 {
    my @args = @_;
    
    return seq(\&rtlast, @args);
}


sub token {
    my $pat         = shift;
    my $description = shift // $pat;
    
    return last1(re('\s*'), re($pat, $description));
}

sub chartk {
    my $c = shift;
    return last1(re('\s*'), char($c));
}


### HELPERS ####
### Attach an element to the front of a list
sub cons {
    my $e   = shift or croak 'No e';
    my $arr = shift or croak 'No arr';

    return [$e, @$arr];
}

### Returns the arguments as a list ref
sub list {
    return \@_;
}

### Returns the last argument
sub rtlast {
    return pop();
}

### Returns the first argument
sub rtfirst {
    return shift();
}

sub rtsecond {
    shift();
    return shift();
}

sub cat {
    return join '', @_;
}

sub consrtree {
    my ($x, $recur) = @_;
    return $x if ! @$recur;
    my ($op, $xtree) = @$recur;
    return [ $op, $x, $xtree ]
}

sub map2ltree {
    my $left  = shift or croak 'No left';
    my $pairs = shift or croak 'No pairs';
    
    my $pair;
    while (defined ($pair = shift @$pairs)) {
        my ($op, $right) = @$pair;
        $left = [$op, $left, $right];
    }
    return $left;
}

###
sub flatten {
    my $tree = shift;
    if ( ref $tree ne 'ARRAY') {
        return $tree;
    }
    else {
        my @list = map {
            ref $_ eq 'ARRAY' ? @{flatten($_)} : $_;
        } @$tree;
        return \@list;
    }
}

## The member(s) if a list that scores the highest according to
## some provided score function.
sub mostn {
    my $fn  = shift or croak 'No fn';   # a -> Number
    my $lst = shift or croak 'No lst';  # [a]
    # returns ([a], Number)
    if (@$lst == 0) {
        return ([], undef);
    }
    my $max    = $fn->($lst->[0]);
    my $score;
    my $len = @$lst;
    my @mosts = $lst->[0];
    foreach my $e (@$lst[1..($len-1)]) {
        $score = $fn->($e);
        if ( $score == $max ) {
            push @mosts, $e;
        }
        elsif ( $score > $max ) {
            @mosts = ($e);
            $max = $score;
        }
    }
    return (\@mosts, $max);
}



####################### Simple calculator ####################################
my $number = token('\d+', "a number");

my $addexp;
my $atom = choice($number,
                  bracket(chartk('('),
                          sub { $addexp }, # Mutual recursion//This is the only way
                          chartk(')')));

my $addop   = token('[+\-]', "'+' or '-'");

my $multop  = token('[*/]', "'*' or '/'");

my $multexp = chainl1($atom, $multop);

$addexp     = chainl1($multexp, $addop);

my $exp     = first1($addexp, re('\s*'));

##################### Logical expression parser ############################
my $ident    = token("[[:alpha:]_-]+", "an identifier");

my $logexp;

my $latom    = choice(bracket(chartk('('),
                              sub { $logexp },
                              chartk(')')),
                      $ident);

my $unaryexp = choice(listseq(token('not'), $latom),
                      $latom);

my $andexp   = chainl1($unaryexp, token('and'));

my $orexp    = chainl1($andexp, token('or'));

$logexp = $orexp;

sub logparse {
    return $logexp->(@_);
}

##################### Lisp like log expressions #############################
my $sexp;

$sexp = choice($ident,
               bracket(chartk('('),
                       cons2(token('and|or|not|none-of'), many(sub { $sexp })),
                       chartk(')')));

sub lisp {
    return $sexp->(@_);
}

##################### Other functions #######################################
sub parse {
    my $inp = shift;
    my $result = $exp->($inp);
    if (ref $result ne 'ARRAY') {
        print "No parse";
        return $result;
    }
    my ($tree, $tail) = @$result;
    
    print "Tail: |$tail|\n";
    return evaluate($tree);
}

{
    my $ops =
      { '+' => sub { shift() + shift() },
        '-' => sub { shift() - shift() },
        '*' => sub { shift() * shift() },
        '/' => sub { shift() / shift() }
      };

    sub evaluate {
        my $tree = shift;
        if (ref $tree eq 'ARRAY') {
            my ($op, $l, $r) = @$tree;
            return $ops->{$op}->(evaluate($l), evaluate($r));
        }
        else {
            return $tree;
        }
    }
}

1;
