use PPK;

use Test::More;
use Test::Deep;

use strict;

## Helper functions
my $id = sub {
    return shift();
};

my $array = sub {
    return [ @_ ];
};

my $number = PPK::re('[[:digit:]]+', 'number');

subtest "Basic non-consuming parsers" => sub {
    cmp_deeply(pure('value')->('body'), ['value', 'body'], "pure parser");

    cmp_deeply(zero('expected')->('body'),
               superhashof({ expected => 'expected', input => 'body'}),
               "zero parser");
};

subtest "Basic consuming parsers" => sub {
    cmp_deeply(char('w')->("walther"), ['w', 'alther'], "char parser succeeding");

    cmp_deeply(char('j')->("walther"),
               superhashof({ expected => 'j', input => 'walther' }),
               "char parser erroring");

    cmp_deeply(char('w')->(''),
               superhashof({ expected => 'w', input => '' }),
               "char parser on empty input");
    
    cmp_deeply($number->('101 Wild Turkey'),
               ['101', ' Wild Turkey'],
               "regex parser succeeding");
    
    cmp_deeply($number->('Wild Turkey 101'),
               superhashof({ expected => 'number', input => 'Wild Turkey 101' }),
               "regex parser failing");

    cmp_deeply(PPK::re(sub { ['Proof', shift()] },
                       '[[:digit:]]+',
                       'US Proof')->('101 Wild Turkey'),
               [['Proof', '101'], ' Wild Turkey'],
               "regex parser with user applied mapping of parsed value");

};

subtest "Basic parser combinations" => sub {
    cmp_deeply(seq($array, char('a'), char('b'), char('c'))->("abcdefg"),
               [['a', 'b', 'c'], 'defg'],
               "sequence parser");

    cmp_deeply(seq($array, char('a'), char('b'), char('x'))->('abc'),
               superhashof( { expected => 'x', input => 'c'} ),
               "sequence parser in error");
    
    cmp_deeply(choice(char('a'), char('b'), char('c'))->('bent'),
               ['b', 'ent'],
               "choice parser");

    cmp_deeply(choice(char('a'), char('b'), char('c'))->('straight'),
               superhashof({ expected => ['a','b','c'], input => 'straight'}),
               "choice parser in error");

    my @seqs = (seq($array, char('a'), char('b'), char('x')),
                seq($array, char('a'), char('y'), char('z')),
                seq($array, char('a'), char('b'), char('c')));

    cmp_deeply(choice(@seqs)->('abcd'),
               [['a', 'b', 'c'], 'd'],
               "choice operator with arbitrary lookahead");

    cmp_deeply(choice(@seqs)->('abhk'),
               superhashof({ expected => ['x','c'], input  => 'hk'}),
               "choice operator with arbitrary lookahead in error");

    cmp_deeply(many(char('x'))->('xxxyyz'),
               [['x','x','x'], 'yyz'],
               "many parser consuming something");
    
    cmp_deeply(many(char('a'))->('xxx'),
               [[], 'xxx'],
               "many parser not consuming anything");
    
    cmp_deeply(many1(char('x'))->('xxxyyz'),
               [['x','x','x'], 'yyz'],
               "many1 parser");
    
    cmp_deeply(many1(char('a'))->('xxx'),
               superhashof( { expected => 'a', input => 'xxx'} ),
               "many1 parser on error");

};

subtest "Repetetive parser combinations" => sub {
    cmp_deeply(sepby1($number, char(','))->('101 Wild Turkey'),
               [['101'], ' Wild Turkey'],
               "sepby1 parser on one occurrence of p");

    cmp_deeply(sepby1($number, char(','))->('1,2,3 and go'),
               [['1','2','3'], ' and go'],
               "sepby1 parser with several occurences");
    
    cmp_deeply(sepby1($number, char(','))->('Wild Turkey'),
               superhashof({ expected => 'number', input => 'Wild Turkey' }),
               "sepby1 parser on one occurrence of p");

    cmp_deeply(endby1($number, char(','))->('1,2,3 and go'),
               [['1','2'], '3 and go'],
               "endby1 parser");
    
    cmp_deeply(endby1($number, char(','))->('Wild Turkey'),
               superhashof({ expected => 'number', input => 'Wild Turkey' }),
               "endby1 parser on error");
    
    cmp_deeply(endby1($number, char(','))->('101'),
               superhashof({ expected => ',', input => ''}),
               "endby1 parser on error - no end token");

};

subtest "Higher level parser combinations" => sub {
    cmp_deeply(chainl1($number, PPK::re('[+\-]'))->("1+2-3+4"),
               [['+', ['-', ['+', '1', '2'], '3'], '4'],''],
               "chainl1 parser (left associative infix)");

    cmp_deeply(chainr1(PPK::re('[a-z]'), PPK::re('[=$]'))->('a$b=c$d'),
               [['$', 'a', ['=', 'b', ['$', 'c', 'd']]], ''],
               "chainr1 parser (right associative infix)");

    cmp_deeply(bracket(char('['), $number, char(']'))->('[15]'),
               ['15', ''],
               "bracket parser");
                   
};

done_testing();
