package PPK;

use strict;
use Carp;
use Ted::Lambda qw( ncurry );

### TODO: General functionality - transfer to a file by itself, or Ted::Lambda
sub do_applicative {
    my $star  = shift or croak 'No star';
    my $pure  = shift or croak 'No pure';
    my $f     = shift or croak 'No f';

    my $cf = $pure->(ncurry($f, scalar @_));
    my $v;
    while (defined ($v = shift @_)) {
        $cf = $star->($cf,$v);
    }
    return $cf;
}


## Parsers
sub seq {
    my $f   = shift or croak 'No f';
    return do_applicative(\&combine, \&pure, $f, @_);
}

sub fmap {
    my $f = shift or croak 'No f';
    my $p = shift or croak 'No p';
    return combine(pure($f), $p);
}

### $v -> ($s -> [$v, $s])
### Pure parser will always succeed without consuming anything
sub pure {
    my $v = shift; croak if !defined $v;
    return sub {
        my $inp = shift;
        return [$v, $inp];
    }
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

# (<*>) :: Parser (a -> a') -> Parser a -> Parser a'
# ff <*> fv = \inp -> do (f', inp')  <- ff inp
#                        (v', inp'') <- fv inp'
#                        return (f' v', inp'')
## Applicative functors
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


## TODO: zero parser should take some error description as paramater.
##       inp doesn't matter since zero will always fail.
### Parser that always fails
sub zero {
    my $expected   = shift;
    
    sub {
        my $got      = shift;
        my $inp      = shift;

        return { type       => 'Parse error',
                 input      => $inp,
                 expected   => flatten($expected),
                 got        => $got
             };
    }
};

### Parses any character, fails if input string is empty
sub item {
    my $inp = shift;
    if ($inp eq '') {
        return zero('item')->(undef, $inp);
    }
    else {
        my ($s1, $s2) = (substr($inp, 0, 1), substr($inp, 1));
        return pure($s1)->($s2);
    }
};


### Parses character $c, otherwise fails
sub char {
    my $c = shift; croak 'No c' if ! defined $c;
    
    sub {
        my $inp = shift;
        my ($s1, $s2) = (substr($inp, 0, 1), substr($inp, 1));
        if ($s1 eq $c) {
            return pure($s1)->($s2);
        }
        else {
            return zero($c)->($s1, $inp);
        }
    }
}

### Regex parser
sub re {
    my $s           = shift or croak 'No s';
    my $description = shift || $s;
    
    my $regex = qr{^($s)(.*)$};
    
    sub {
        my $inp = shift;
        my ($s1,$s2) = $inp =~ m{$regex};
        if (defined $1) {
            return pure($s1)->($s2);
        }
        else {
            return zero($description)->($s1, $inp);
        }
    };
}

## If parser $p succeeds, then test the result with $pred
## Only succeeds of $pred returns a true value
sub predicate {
    my $pred = shift or croak 'No pred';
    my $p    = shift or croak 'No p';

    return bindp($p, sub {
                     my $v = shift;
                     if ($pred->($v)) {
                         return pure($v);
                     }
                     else {
                         return zero("Parsed value $v failed predicate");
                     }
                 });
}


### Try a list of parsers. The results of all parses that succeed are collected
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
sub last1 {
    my @args = @_;
    
    return seq(\&rtlast, @args);
}

sub first1 {
    return seq(\&rtfirst, @_);
}

sub second1 {
    return seq(\&rtsecond, @_);
}

sub listseq {
    return seq(\&list, @_);
}

sub cons2 {
    my ($p1, $p2) = @_;
    return seq(\&cons, $p1, $p2);
}

## TODO: Make it into a while loop instead
sub many {
    my $p  = shift or croak 'No p';

    return choice(cons2($p, recur(\&many, $p)),
                  pure([]));
}

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

## Right associative operators
sub chainr1 {
    my $p  = shift or croak 'No p';
    my $s  = shift or croak 'No s';

    return seq(\&consrtree,
               $p,
               choice(listseq($s, recur(\&chainr1, $p, $s)),
                      pure([])));
}

### Left associative operators
sub chainl1 {
    my $p = shift or croak 'No p';
    my $s = shift or croak 'No s';

    return seq(\&map2ltree, $p, many(listseq($s, $p)));
}

sub bracket {
    my $open  = shift or croak 'No open';
    my $p     = shift or croak 'No p';
    my $close = shift or croak 'No close';

    return seq(\&rtsecond, $open, $p, $close);
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

##################### Parser helpsers #######################################
sub token {
    my $pat         = shift;
    my $description = shift || $pat;
    
    return second1(re('\s*'), re($pat, $description));
}

sub chartk {
    my $c = shift;
    return second1(re('\s*'), char($c));
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
