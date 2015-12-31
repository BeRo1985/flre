unit UnitReplaceTests;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses UnitTestGlobals,FLRE;

procedure ExecuteReplaceTests;

implementation

procedure ExecuteReplaceTests;
begin
 ExecuteReplaceTest('(qu|[b-df-hj-np-tv-z]*)([a-z]+)',
                    '\2\1ay',
                    'the quick brown fox jumps over the lazy dogs.',
                    'ethay quick brown fox jumps over the lazy dogs.',
                    'ethay ickquay ownbray oxfay umpsjay overay ethay azylay ogsday.',
                    9,
                    []);
 ExecuteReplaceTest('\w+',
                    '\0-NOSPAM',
                    'abcd.efghi@google.com',
                    'abcd-NOSPAM.efghi@google.com',
                    'abcd-NOSPAM.efghi-NOSPAM@google-NOSPAM.com-NOSPAM',
                    4,
                    []);
 ExecuteReplaceTest('^',
                    '(START)',
                    'foo',
                    '(START)foo',
                    '(START)foo',
                    -1,
                    []);
 ExecuteReplaceTest('^',
                    '(START)',
                    '',
                    '(START)',
                    '(START)',
                    -1,
                    []);
 ExecuteReplaceTest('$',
                    '(END)',
                    'foo',
                    'foo(END)',
                    'foo(END)',
                    -1,
                    []);
 ExecuteReplaceTest('$',
                    '(END)',
                    '',
                    '(END)',
                    '(END)',
                    -1,
                    []);
 ExecuteReplaceTest('b',
                    'bb',
                    'ababababab',
                    'abbabababab',
                    'abbabbabbabbabb',
                    5,
                    []);
 ExecuteReplaceTest('b',
                    'bb',
                    'bbbbbb',
                    'bbbbbbb',
                    'bbbbbbbbbbbb',
                    6,
                    []);
 ExecuteReplaceTest('b+',
                    'bb',
                    'bbbbbb',
                    'bb',
                    'bb',
                    1,
                    []);
 ExecuteReplaceTest('b*',
                    'bb',
                    'bbbbbb',
                    'bb',
                    'bb',
                    1,
                    []);
 ExecuteReplaceTest('b*',
                    'bb',
                    'aaaaa',
                    'bbaaaaa',
                    'bbabbabbabbabbabb',
                    6,
                    []);
 ExecuteReplaceTest('a.*a',
                    '(\0)',
                    'aba'#10'aba',
                    '(aba)'#10'aba',
                    '(aba)'#10'(aba)',
                    2,
                    []);
end;

end.
