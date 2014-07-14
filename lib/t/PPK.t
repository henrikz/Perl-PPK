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
    
    cmp_deeply(PPK::re('[[:digit:]]+')->('101 Wild Turkey'),
               ['101', ' Wild Turkey'],
               "regex parser succeeding");
    
    cmp_deeply(PPK::re('[[:digit:]]', 'number')->('Wild Turkey 101'),
               superhashof({ expected => 'number', input => 'Wild Turkey 101' }),
               "regex parser failing");
    
};

subtest "Basic parser combination" => sub {
    cmp_deeply(seq($array, char('a'), char('b'), char('c'))->("abcdefg"),
               [['a', 'b','c'], 'defg'],
               "Sequencing");
                   
    ## Choice ...
};


done_testing();
