unit UnitSearchTests;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses UnitTestGlobals,FLRE;

procedure ExecuteSearchTests;

implementation

procedure ExecuteSearchTests;
begin

 // Simple basic tests
 ExecuteSearchTest('a','a',[]);
 ExecuteSearchTest('a','zyzzyva',[]);
 ExecuteSearchTest('a+','aa',[]);
 ExecuteSearchTest('(a+|b)+','ab',[]);
 ExecuteSearchTest('ab|cd','xabcdx',[]);
 ExecuteSearchTest('h.*od?','hello'#10'goodbye'#10,[]);
 ExecuteSearchTest('h.*o','hello'#10'goodbye'#10,[]);
 ExecuteSearchTest('h.*o','goodbye'#10'hello'#10,[]);
 ExecuteSearchTest('h.*o','hello world',[]);
 ExecuteSearchTest('h.*o','othello, world',[]);
 ExecuteSearchFailTest('[^\s\S]','aaaaaaa',[]);
 ExecuteSearchTest('a','aaaaaaa',[]);
 ExecuteSearchTest('a*','aaaaaaa',[]);
 ExecuteSearchTest('a*','',[]);
 ExecuteSearchTest('a','cab',[]);
 ExecuteSearchTest('a*b','cab',[]);
 ExecuteSearchTest('((((((((((((((((((((x))))))))))))))))))))','x',[]);
 ExecuteSearchTest('[abcd]','xxxabcdxxx',[]);
 ExecuteSearchTest('[^x]','xxxabcdxxx',[]);
 ExecuteSearchTest('[abcd]+','xxxabcdxxx',[]);
 ExecuteSearchTest('[^x]+','xxxabcdxxx',[]);
 ExecuteSearchTest('(fo|foo)','fo',[]);
 ExecuteSearchTest('(foo|fo)','foo',[]);
 ExecuteSearchTest('aa','aA',[rfIGNORECASE]);
 ExecuteSearchFailTest('aa','aA',[]);
 ExecuteSearchTest('a','Aa',[rfIGNORECASE]);
 ExecuteSearchTest('ABC','abc',[rfIGNORECASE]);
 ExecuteSearchFailTest('ABC','abc',[]);
 ExecuteSearchTest('abc','XABCY',[rfIGNORECASE]);
 ExecuteSearchFailTest('abc','XABCY',[]);
 ExecuteSearchTest('ABC','xabcy',[rfIGNORECASE]);
 ExecuteSearchFailTest('ABC','xabcy',[]);

 // Make sure ^ and $ work.
 ExecuteSearchTest('foo|bar|[A-Z]','foo',[]);
 ExecuteSearchTest('^(foo|bar|[A-Z])','foo',[]);
 ExecuteSearchTest('(foo|bar|[A-Z])$','foo'#10,[rfMULTILINE]);
 ExecuteSearchTest('(foo|bar|[A-Z])$','foo',[]);
 ExecuteSearchTest('^(foo|bar|[A-Z])$','foo'#10,[rfMULTILINE]);
 ExecuteSearchTest('^(foo|bar|[A-Z])$','foo',[]);
 ExecuteSearchTest('^(foo|bar|[A-Z])$','bar'#10,[rfMULTILINE]);
 ExecuteSearchTest('^(foo|bar|[A-Z])$','X',[]);
 ExecuteSearchFailTest('^(foo|bar|[A-Z])$','XY',[]);
 ExecuteSearchFailTest('(foo|bar|[A-Z])$','foo '#10,[rfMULTILINE]);
 ExecuteSearchFailTest('(foo|bar|[A-Z])$','foo ',[]);
 ExecuteSearchFailTest('^(foo|bar|[A-Z])$',' foo '#10,[rfMULTILINE]);
 ExecuteSearchFailTest('^(foo|bar|[A-Z])$',' foo ',[]);
 ExecuteSearchTest('^$','',[]);
 ExecuteSearchFailTest('^$','x',[]);
 ExecuteSearchTest('^^$','',[]);
 ExecuteSearchTest('^$$','',[]);
 ExecuteSearchFailTest('^^$','x',[]);
 ExecuteSearchFailTest('^$$','x',[]);
 ExecuteSearchTest('^^$$','',[]);
 ExecuteSearchFailTest('^^$$','x',[]);
 ExecuteSearchTest('^^^^^^^^$$$$$$$$','',[]);
 ExecuteSearchTest('^','x',[]);
 ExecuteSearchTest('$','x',[]);
 ExecuteSearchAnchoredTest('^','x',[]);
 ExecuteSearchAnchoredFailTest('$','x',[]);

 // Word boundaries
 ExecuteSearchTest('\bfoo\b', 'nofoo foo that',[]);
 ExecuteSearchTest('a\b', 'faoa x',[]);
 ExecuteSearchTest('\bbar', 'bar x',[]);
 ExecuteSearchTest('\bbar', 'foo'#10'bar x',[]);
 ExecuteSearchTest('bar\b', 'foobar',[]);
 ExecuteSearchTest('bar\b', 'foobar'#10'xxx',[]);
 ExecuteSearchTest('(foo|bar|[A-Z])\b', 'foo',[]);
 ExecuteSearchTest('(foo|bar|[A-Z])\b', 'foo'#10,[]);
 ExecuteSearchFailTest('\b', '',[]);
 ExecuteSearchTest('\b', 'x',[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])', 'foo',[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])\b', 'X',[]);
 ExecuteSearchFailTest('\b(foo|bar|[A-Z])\b', 'XY',[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])\b', 'bar',[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])\b', 'foo',[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])\b', 'foo'#10,[]);
 ExecuteSearchTest('\b(foo|bar|[A-Z])\b', 'ffoo bbar N x',[]);
 ExecuteSearchTest('\b(fo|foo)\b', 'fo',[]);
 ExecuteSearchTest('\b(fo|foo)\b', 'foo',[]);
 ExecuteSearchFailTest('\b\b', '',[]);
 ExecuteSearchTest('\b\b', 'x',[]);
 ExecuteSearchFailTest('\b$', '',[]);
 ExecuteSearchTest('\b$', 'x',[]);
 ExecuteSearchTest('\b$', 'y x',[]);
 ExecuteSearchTest('\b.$', 'x',[]);
 ExecuteSearchTest('^\b(fo|foo)\b', 'fo',[]);
 ExecuteSearchTest('^\b(fo|foo)\b', 'foo',[]);
 ExecuteSearchFailTest('^\b', '',[]);
 ExecuteSearchTest('^\b', 'x',[]);
 ExecuteSearchFailTest('^\b\b', '',[]);
 ExecuteSearchTest('^\b\b', 'x',[]);
 ExecuteSearchFailTest('^\b$', '',[]);
 ExecuteSearchFailTest('^\b$', 'x',[]);
 ExecuteSearchTest('^\b.$', 'x',[]);
 ExecuteSearchTest('^\b.\b$', 'x',[]);
 ExecuteSearchFailTest('^^^^^^^^\b$$$$$$$', '',[]);
 ExecuteSearchTest('^^^^^^^^\b.$$$$$$', 'x',[]);
 ExecuteSearchFailTest('^^^^^^^^\b$$$$$$$', 'x',[]);

// Non-word boundaries.
 ExecuteSearchTest('\Bfoo\B', 'n foo xfoox that',[]);
 ExecuteSearchTest('a\B', 'faoa x',[]);
 ExecuteSearchFailTest('\Bbar', 'bar x',[]);
 ExecuteSearchFailTest('\Bbar', 'foo'#10'bar x',[]);
 ExecuteSearchFailTest('bar\B', 'foobar',[]);
 ExecuteSearchFailTest('bar\B', 'foobar'#10'xxx',[]);
 ExecuteSearchTest('(foo|bar|[A-Z])\B', 'foox',[]);
 ExecuteSearchFailTest('(foo|bar|[A-Z])\B', 'foo'#10,[]);
 ExecuteSearchTest('\B', '',[]);
 ExecuteSearchFailTest('\B', 'x',[]);
 ExecuteSearchFailTest('\B(foo|bar|[A-Z])', 'foo',[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])', 'foobar',[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])\B', 'xXy',[]);
 ExecuteSearchFailTest('\B(foo|bar|[A-Z])\B', 'XY',[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])\B', 'XYZ',[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])\B', 'abara',[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])\B', 'xfoo_',[]);
 ExecuteSearchFailTest('\B(foo|bar|[A-Z])\B', 'xfoo'#10,[]);
 ExecuteSearchTest('\B(foo|bar|[A-Z])\B', 'foo bar vNx',[]);
 ExecuteSearchTest('\B(fo|foo)\B', 'xfoo',[]);
 ExecuteSearchTest('\B(foo|fo)\B', 'xfooo',[]);
 ExecuteSearchTest('\B\B', '',[]);
 ExecuteSearchFailTest('\B\B', 'x',[]);
 ExecuteSearchTest('\B\B', 'xx',[]);
 ExecuteSearchTest('\B$', '',[]);
 ExecuteSearchFailTest('\B$', 'x',[]);
 ExecuteSearchFailTest('\B$', 'y x',[]);
 ExecuteSearchFailTest('\B.$', 'x',[]);
 ExecuteSearchFailTest('^\B(fo|foo)\B', 'fo',[]);
 ExecuteSearchFailTest('^\B(fo|foo)\B', 'foo',[]);
 ExecuteSearchTest('^\B', '',[]);
 ExecuteSearchFailTest('^\B', 'x',[]);
 ExecuteSearchTest('^\B\B', '',[]);
 ExecuteSearchFailTest('^\B\B', 'x',[]);
 ExecuteSearchTest('^\B$', '',[]);
 ExecuteSearchFailTest('^\B$', 'x',[]);
 ExecuteSearchFailTest('^\B.$', 'x',[]);
 ExecuteSearchFailTest('^\B.\B$', 'x',[]);
 ExecuteSearchTest('^^^^^^^^\B$$$$$$$', '',[]);
 ExecuteSearchFailTest('^^^^^^^^\B.$$$$$$', 'x',[]);
 ExecuteSearchFailTest('^^^^^^^^\B$$$$$$$', 'x',[]);

 // Weird boundary cases.
 ExecuteSearchTest('^$^$','',[]);
 ExecuteSearchTest('^$^','',[]);
 ExecuteSearchTest('$^$','',[]);
 ExecuteSearchFailTest('^$^$','x',[]);
 ExecuteSearchFailTest('^$^','x',[]);
 ExecuteSearchFailTest('$^$','x',[]);
 ExecuteSearchFailTest('^$^$','x'#10'y',[]);
 ExecuteSearchFailTest('^$^','x'#10'y',[]);
 ExecuteSearchFailTest('$^$','x'#10'y',[]);
 ExecuteSearchFailTest('^$^$','x'#10#10'y',[]);
 ExecuteSearchFailTest('^$^','x'#10#10'y',[]);
 ExecuteSearchFailTest('$^$','x'#10#10'y',[]);
 ExecuteSearchFailTest('^(foo\$)$','foo$bar',[]);
 ExecuteSearchTest('^(foo\$)','foo$',[]);
 ExecuteSearchTest('(foo\$)','foo$',[]);
 ExecuteSearchTest('^...$','abc',[]);

 // UTF-8
 ExecuteSearchTest('^'#$e6#$9c#$ac'$',#$e6#$9c#$ac,[rfUTF8]);
 ExecuteSearchTest('^...$',#$e6#$97#$a5#$e6#$9c#$ac#$e8#$aa#$9e,[rfUTF8]);
 ExecuteSearchTest('^...$','.'#$e6#$9c#$ac'.',[rfUTF8]);
 ExecuteSearchTest('^\C\C\C$',#$e6#$9c#$ac,[rfUTF8]);
 ExecuteSearchFailTest('^\C$',#$e6#$9c#$ac,[rfUTF8]);
 ExecuteSearchFailTest('^\C\C\C$',#$e6#$97#$a5#$e6#$9c#$ac#$e8#$aa#$9e,[rfUTF8]);

 // Latin1
 ExecuteSearchFailTest('^...$', #$e6#$97#$a5#$e6#$9c#$ac#$e8#$aa#$9e,[]);
 ExecuteSearchTest('^.........$', #$e6#$97#$a5#$e6#$9c#$ac#$e8#$aa#$9e,[]);
 ExecuteSearchFailTest('^...$', '.'#$e6#$9c#$ac'.',[]);
 ExecuteSearchTest('^.....$', '.'#$e6#$9c#$ac'.',[]);

end;

end.

