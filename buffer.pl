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

### $v -> ($s -> [[$v, $s]])
### Pure parser will always succeed without consuming anything
sub pure {
    my $v = shift; croak if !defined $v;
    return sub {
        my $s = shift;
        return [[$v, $s]];
    }
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
        my @res = map {
            my ($fm, $inpm) = @$_;
            my $vs = $fv->($inpm);
            ## Possible lazyness
            if (ref $vs eq 'CODE') {
                $vs = $vs->($inpm);
            }
            map {
                my ($vm, $inpmm) = @$_;
                [$fm->($vm), $inpmm];
            } @$vs;
        } @$fs;
        return \@res;
    };
}

### Make a lazy parser
### Example
###    strict: many($p)
##
###    lazy  : recur(\&many, $p)
### This is necessary when constructing recursive parsers
sub recur {
    my $f    = shift or croak 'No f';
    my $args = \@_;
    my $p    = undef;

    sub {
        # Optimization, don't know if it works
        $p = $f->(@$args) if ! defined $p;
        return $p;
    };
}

### TODO: End of file, and not end of file

### Parser that always fails
sub zero {
    return [];
};

### Parses any character, fails if input string is empty
sub item {
    my $inp = shift;
    if ($inp eq '') {
        return zero();
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
            return zero();
        }
    }
}

### Regex parser
sub re {
    my $s     = shift or croak 'No s';
    my $regex = qr{^($s)(.*)$};
    
    sub {
        my $inp = shift;
        my ($s1,$s2) = $inp =~ m{$regex};
        if (defined $1) {
            return pure($s1)->($s2);
        }
        else {
            return zero();
        }
    };
}

### Try a list of parsers. The results of all parses that succeed are collected
sub choice {
    my @parsers = @_;
    sub {
        my $inp = shift;
        my $result = undef;

        for my $p(@parsers) {
            my $st = $p->($inp);
            if (ref $st eq 'CODE') {
                $st = $st->($inp);
            }
            if (@$st) {
                return $st;
            }
        }
        return zero($inp);
    }
}

## Ideas for support functions: list, last, number, etc.
sub last1 {
    return seq(\&rtlast, @_);
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
    
    unshift @$arr, $e;
    return $arr;
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



####################### Simple calculator ####################################
my $number = token('\d+');

my $addexp;
my $atom = choice($number,
                  bracket(chartk('('),
                          sub { $addexp }, # Mutual recursion//This is the only way
                          chartk(')')));

my $addop   = token('[+\-]');

my $multop  = token('[*/]');

my $multexp = chainl1($atom, $multop);

$addexp     = chainl1($multexp, $addop);

my $exp     = first1($addexp, re('\s*'));

##################### Parser helpsers #######################################
sub token {
    my $pat = shift;
    return second1(re('\s*'), re($pat));
}

sub chartk {
    my $c = shift;
    return second1(re('\s*'), char($c));
}

##################### Other functions #######################################
sub parse {
    my $inp = shift;
    my $results = $exp->($inp);
    if (!@$results) { croak 'No parse'; }
    my $result  = shift @$results;
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
