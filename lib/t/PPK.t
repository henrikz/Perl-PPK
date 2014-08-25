use PPK;

use Test::More;
use Test::Deep;

use strict;
use Carp;
use Try::Tiny;

## Helper functions
my $id = sub {
    return shift();
};

my $array = sub {
    return [ @_ ];
};

my $number = PPK::re('[[:digit:]]+', 'number');

### Basic test tool for parsers, that ensures a uniform way of testing.
sub test_parse {
    my ($arg1) = @_;
    my ($parser, $input, $expected, $unparsed, $description) = ();
    my $arg1_type = ref $arg1;

    if ($arg1_type eq 'CODE') {
        ## Simple non-named args
        $parser      = shift or croak 'No parser';
        $input       = shift // croak 'No string';
        $expected    = shift // croak 'No expected';
        $description = shift or croak 'No description';
    }
    else {
        ## Named args
        my %args = @_;
        $parser      = $args{parser}      or croak 'No parser';
        $input       = $args{input}       // croak 'No input';
        $unparsed    = $args{unparsed};   # Optional
        $expected    = $args{expected}    // croak 'No expected';
        $description = $args{description} or croak 'No description';
    }
    
    my $result   = $parser->($input);
    if (ref $expected eq 'HASH') {
        ## Test expected error parse
        cmp_deeply($result, superhashof($expected), $description);
    }
    else {
        if ($unparsed) {
            ## Test both parse value, and the rest of the input string
            cmp_deeply($result, [$expected, $unparsed], $description);
        }
        else {
            ## Test just the parse value
            cmp_deeply($result->[0], $expected, $description);
        }
    }
}


subtest "Basic non-consuming parsers" => sub {

    test_parse(parser      => pure('value'),
               input       => 'body',
               expected    => 'value',
               unparsed    => 'body',
               description => "pure parser");

    test_parse(zero('expected'),
               'body',
               { expected => 'expected', input => 'body'},
               "zero parser");
};

subtest "Basic consuming parsers" => sub {
    test_parse(parser      => char('w'),
               input       => "walther",
               expected    => 'w',
               unparsed    => 'alther',
               description => "char parser succeeding");
    
    test_parse(char('j'),
               "walther",
               { expected => 'j', input => 'walther' },
               "char parser erroring");

    test_parse(char('w'),
               '',
               { expected => 'w', input => '' },
               "char parser on empty input");
    
    test_parse(parser      => $number,
               input       => '101 Wild Turkey',
               expected    => '101',
               unparsed    => ' Wild Turkey',
               description => "regex parser succeeding");
    
    test_parse($number, 'Wild Turkey 101',
               { expected => 'number', input => 'Wild Turkey 101' },
               "regex parser failing");

    test_parse(parser      => PPK::re(sub { sprintf 'Proof: %s', shift() },
                                      '[[:digit:]]+',
                                      'US Proof'),
               input       => '101 Wild Turkey',
               expected    => 'Proof: 101',
               unparsed    => ' Wild Turkey',
               description => "regex parser with user applied mapping of parsed value");
    
};

subtest "Basic parser combinations" => sub {
    test_parse(parser       => seq($array, char('a'), char('b'), char('c')),
               input        => "abcdefg",
               expected     => ['a', 'b', 'c'],
               unparsed     => 'defg',
               description  => "sequence parser");

    test_parse(seq($array, char('a'), char('b'), char('x')), 'abc',
               { expected => 'x', input => 'c'},
               "sequence parser in error");
    
    test_parse(parser      => choice(char('a'), char('b'), char('c')),
               input       => 'bent',
               expected    => 'b',
               unparsed    => 'ent',
               description => "choice parser");
    
    test_parse(choice(char('a'), char('b'), char('c')), 'straight',
               { expected => ['a','b','c'], input => 'straight'},
               "choice parser in error");

    my @seqs = (seq($array, char('a'), char('b'), char('x')),
                seq($array, char('a'), char('y'), char('z')),
                seq($array, char('a'), char('b'), char('c')));

    test_parse(parser      => choice(@seqs),
               input       => 'abcd',
               expected    => ['a', 'b', 'c'],
               unparsed    => 'd',
               description => "choice operator with arbitrary lookahead");

    test_parse(choice(@seqs), 'abhk',
               { expected => ['x','c'], input  => 'hk'},
               "choice operator with arbitrary lookahead in error");

    test_parse(parser      => many(char('x')),
               input       => 'xxxyyz',
               expected    => ['x','x','x'],
               unparsed    => 'yyz',
               description => "many parser consuming something");
    
    test_parse(parser      => many(char('a')),
               input       => 'xxx',
               expected    => [],
               unparsed    => 'xxx',
               description => "many parser not consuming anything");
    
    test_parse(parser      => many1(char('x')),
               input       => 'xxxyyz',
               expected    => ['x','x','x'],
               unparsed    => 'yyz',
               description => "many1 parser");
    
    test_parse(many1(char('a')), 'xxx',
               { expected => 'a', input => 'xxx'},
               "many1 parser on error");

};

subtest "Repetetive parser combinations" => sub {
    test_parse(parser      => sepby1($number, char(',')),
               input       => '101 Wild Turkey',
               expected    => ['101'],
               unparsed    => ' Wild Turkey',
               description => "sepby1 parser on one occurrence of p");

    test_parse(parser      => sepby1($number, char(',')),
               input       => '1,2,3 and go',
               expected    => ['1','2','3'],
               unparsed    => ' and go',
               description => "sepby1 parser with several occurences");

    
    test_parse(sepby1($number, char(',')) ,'Wild Turkey',
               { expected => 'number', input => 'Wild Turkey' },
               "sepby1 parser on one occurrence of p");

    test_parse(parser      => endby1($number, char(',')),
               input       => '1,2,3 and go',
               expected    => ['1','2'],
               unparsed    => '3 and go',
               description => "endby1 parser");
    
    test_parse(endby1($number, char(',')),'Wild Turkey',
               { expected => 'number', input => 'Wild Turkey' },
               "endby1 parser on error");
    
    test_parse(endby1($number, char(',')),'101',
               { expected => ',', input => ''},
               "endby1 parser on error - no end token");

};

subtest "Higher level parser combinations" => sub {
    test_parse(PPK::chainl1($number, PPK::re('[+\-]')), "1+2-3+4",
               ['+', ['-', ['+', '1', '2'], '3'], '4'],
               "PPK::chainl1 parser (left associative infix)");

    test_parse(PPK::chainr1(PPK::re('[a-z]'), PPK::re('[=$]')), 'a$b=c$d',
               ['$', 'a', ['=', 'b', ['$', 'c', 'd']]],
               "PPK::chainr1 parser (right associative infix)");

    test_parse(bracket(char('['), $number, char(']')), '[15]',
               '15',
               "bracket parser");
                   
};


sub arithmetic {
    my $p = shift or croak 'No p';
    return expression($p,
                      right(pure('$')),
                      prefix(choice(chartkn('+'), chartkn('-'))),
                      left(chartkn('^')),
                      left(choice(chartkn('*'), chartkn('/'))),
                      left(choice(chartkn('+'), chartkn('-'))),
                      right(chartkn('&')),
                      right(chartkn('|')),
                      right(chartkn('$')));
}

subtest "expression parser" => sub {
    my $exp = arithmetic(token('[[:digit:]]+', 'number'));

    test_parse(parser      => $exp,
               input       => ' 1 ',
               expected    => 1,
               unparsed    => ' ',
               description => "atomic expression");

    test_parse($exp,
               '--+22',
               ['-',['-',['+', 22]]],
               "prefix operator");

    test_parse($exp, '1 + 2 * 3 + 4 * 5 ^ 6',
               ['+',['+',1,['*',2,3]],['*',4,['^',5,6]]],
               "left associative infix with precedence");

    test_parse($exp, '1 | 2 & 3 & 4 | 5',
               ['|',1,['|',['&',2,['&',3,4]],5]],
               "right associative infix with precedence");

    test_parse($exp, '11 + 7 & 8 ^ - 3 | 19',
               ['|',['&',['+',11,7],['^',8,['-', 3]]],19],
               "left and right infix, and prefix operators");

    my $bracket_exp;
    $bracket_exp = arithmetic(choice(bracket(chartkn('('), sub { $bracket_exp }, chartkn(')')),
                                     token('[[:digit:]]+', 'number'),
                                     token('[[:alpha:]]+', 'identifier')));

    test_parse($bracket_exp, '11 + 7 & 8 ^ - 3 | 19',
               ['|',['&',['+',11,7],['^',8,['-', 3]]],19],
               "left and right infix, and prefix operators with brackets allowed");

    test_parse(parser      => $bracket_exp,
               input       => ' ( 1 + 2 ) * f 4 ^ ( 5 / 6 ) ',
               expected    => ['*',['+',1,2],['^',['$','f',4],['/',5,6]]],
               unparsed    => ' ',
               description => "bracketed expressions, using recursive expression parser");
               
    
};

subtest "the helper function parse()" => sub {
    my $exp    = arithmetic(token('[[:digit:]]+', 'number'));

    cmp_deeply(parse($exp, '1+2+3'), ['+', ['+', 1, 2], 3], "Function parse() succesful");

    my $message = '';
    try {
        parse($exp, '1+2+3 blah');
    } catch {
        $message = $_;
    };

    ok($message =~ m|\[ blah\]|,    "checking error message from parse() for not reaching end-of-input");

    ok($message =~ m|end-of-input|, "checking error message from parse() for not reaching end-of-input /2");

    ok($message =~ m|Parse error|, "checking error message from parse() for not reaching end-of-input /2");

    cmp_deeply(parse($exp, '1+2+3 blah', sub { shift() }),
               superhashof({ input => ' blah', expected => '>end-of-input<'}),
               "parse() function with a supplied error function");

};


### TODO: Thorough testing of recursion

done_testing();
