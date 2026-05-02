unit basics;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

procedure MatchDateRegex;
procedure MatchLazyDog;
procedure SplitStrings;
procedure ExtractAllStrings;
procedure NamedCapturingGroups;
procedure FindPosition;
procedure Test;
procedure Replace;
procedure ConvertedExpressions;

implementation

uses
  SysUtils,
  Generics.Collections,
  FLRE in '..\src\FLRE.pas',
  PUCU in '..\src\PUCU.pas';

procedure _WriteLnTabTab(const AInput: String);
begin
  WriteLn(#9 + #9 + AInput);
end;

procedure _WriteLnTab(const AInput: String);
begin
  WriteLn(#9 + AInput);
end;

procedure MatchDateRegex;
const
  RegexPattern = '\d{1,2}\.\s\w+\s\d{4}';
  TestStrings: TArray<String> = ['17. June 2016', '13. November 2016 is before 17. March 2020',  'Date: 14. June 2016', '1. April 2016 is a joke day', '123. November 202x', '8.April 1999'];
var
  FLREInstance: TFLRE;
  Captures: TFLRECaptures;
  MultiCaptures: TFLREMultiCaptures;
  RegularExpressionFlags: TFLREFlags;
  TestString: String;
  i, j: Integer;
begin
  FLREInstance := TFLRE.Create(RegexPattern, []);

  _WriteLnTab('- Only anchored Result(s) with Match -');
  for TestString in TestStrings do
  begin
    if FLREInstance.Match(TestString, Captures, 1) then
    begin
      for i := Low(Captures) to High(Captures) do
      begin
        _WriteLnTabTab(Copy(TestString, Captures[i].Start, Captures[i].Length));
      end;
      // -- equivalent as only one match is fetched
      //_WriteLnTabTab(Copy(TestString, Captures[0].Start, Captures[0].Length));
    end;
  end;
  // reset
  SetLength(Captures, 0);

  _WriteLnTab('- Only first Result with MatchAll -');
  for TestString in TestStrings do
  begin
    if FLREInstance.MatchAll(TestString, MultiCaptures, 1, 1) then
    begin
      for i := Low(MultiCaptures) to High(MultiCaptures) do
      begin
        for j := Low(MultiCaptures[i]) to High(MultiCaptures[i]) do
        begin
          _WriteLnTabTab(Copy(TestString, MultiCaptures[i][j].Start, MultiCaptures[i][j].Length));
        end;
      end;
    end;
  end;
  // reset
  SetLength(MultiCaptures, 0);

  _WriteLnTab('- All Results with MatchAll -');
  for TestString in TestStrings do
  begin
    if FLREInstance.MatchAll(TestString, MultiCaptures, 1, -1) then
    begin
      for i := Low(MultiCaptures) to High(MultiCaptures) do
      begin
        for j := Low(MultiCaptures[i]) to High(MultiCaptures[i]) do
        begin
          _WriteLnTabTab(Copy(TestString, MultiCaptures[i][j].Start, MultiCaptures[i][j].Length));
        end;
      end;
    end;
  end;
  // reset
  SetLength(MultiCaptures, 0);
end;

procedure MatchLazyDog;
const
  RegexPattern = '\w+o\w+';
  TestString: String = 'The quick brown fox jumps over the lazy dog';
var
  FLREInstance: TFLRE;
  Captures: TFLRECaptures;
  MultiCaptures: TFLREMultiCaptures;
  RegularExpressionFlags: TFLREFlags;
  i, j: Integer;
begin
  FLREInstance := TFLRE.Create(RegexPattern, []);

  _WriteLnTab('- Only anchored first Result(s) with Match -');
  if FLREInstance.Match(TestString, Captures) then
  begin
    // -- equivalent as only one match is fetched
    {
    for i := Low(Captures) to High(Captures) do
    begin
      _WriteLnTabTab(Copy(TestString, Captures[i].Start, Captures[i].Length));
    end;
    }
    _WriteLnTabTab(Copy(TestString, Captures[0].Start, Captures[0].Length));
  end;
  // reset
  SetLength(Captures, 0);

  _WriteLnTab('- Only first Result with MatchNext -');
  if FLREInstance.MatchNext(TestString, Captures) then
  begin
    // -- equivalent as only one match is fetched
    for i := Low(Captures) to High(Captures) do
    begin
      _WriteLnTabTab(Copy(TestString, Captures[i].Start, Captures[i].Length));
    end;
    //_WriteLnTabTab(Copy(TestString, Captures[0].Start, Captures[0].Length));
  end;
  // reset
  SetLength(Captures, 0);

  _WriteLnTab('- Only first Result with MatchAll -');
  if FLREInstance.MatchAll(TestString, MultiCaptures, 1, 1) then
  begin
    for i := Low(MultiCaptures) to High(MultiCaptures) do
    begin
      for j := Low(MultiCaptures[i]) to High(MultiCaptures[i]) do
      begin
        _WriteLnTabTab(Copy(TestString, MultiCaptures[i][j].Start, MultiCaptures[i][j].Length));
      end;
    end;
  end;
  // reset
  SetLength(MultiCaptures, 0);

  _WriteLnTab('- All Results with MatchAll -');
  if FLREInstance.MatchAll(TestString, MultiCaptures, 1, -1) then
  begin
    for i := Low(MultiCaptures) to High(MultiCaptures) do
    begin
      for j := Low(MultiCaptures[i]) to High(MultiCaptures[i]) do
      begin
        _WriteLnTabTab(Copy(TestString, MultiCaptures[i][j].Start, MultiCaptures[i][j].Length));
      end;
    end;
  end;
  // reset
  SetLength(MultiCaptures, 0);
end;

// broken?
procedure SplitStrings;
var
  FLREInstance: TFLRE;
  SplittedStrings: TFLREStrings;
  i: Integer;
begin
  FLREInstance := TFLRE.Create('\s+', []);
  try
    if FLREInstance.Split('Hello world     this is a test', SplittedStrings) then
    begin
      // you can use both to get the indexes
      //for i := Low(SplittedStrings) to High(SplittedStrings) do
      for i := 0 to Length(SplittedStrings) - 1 do
      begin
        _WriteLnTab(SplittedStrings[i]);
      end;
    end;

    SetLength(SplittedStrings, 0);
  finally
    FLREInstance.Free;
  end;


  FLREInstance := TFLRE.Create('\s', []);
  try
    if FLREInstance.Split('The quick brown fox jumps over the lazy dog', SplittedStrings) then
    begin
      for i := 0 to Length(SplittedStrings) - 1 do
      begin
        _WriteLnTab(SplittedStrings[i]);
      end;
    end;

    SetLength(SplittedStrings, 0);
  finally
    FLREInstance.Free;
  end;


  FLREInstance := TFLRE.Create('\d+\s+\w+', []);
  try
    if FLREInstance.Split('12 April', SplittedStrings) then
    begin
      for i := 0 to Length(SplittedStrings) - 1 do
      begin
        _WriteLnTab(SplittedStrings[i]);
      end;
    end;

    SetLength(SplittedStrings, 0);
  finally
    FLREInstance.Free;
  end;


  FLREInstance := TFLRE.Create('\d{1,2}', []);
  try
    if FLREInstance.Split('12. April, 25. May, 30. June', SplittedStrings) then
    begin
      for i := 0 to Length(SplittedStrings) - 1 do
      begin
        _WriteLnTab(SplittedStrings[i]);
      end;
    end;

    SetLength(SplittedStrings, 0);
  finally
    FLREInstance.Free;
  end;
end;

procedure ExtractAllStrings;
var
  FLREInstance: TFLRE;
  MultiExtractions: TFLREMultiStrings;
  i, j: Integer;
begin
  FLREInstance := TFLRE.Create('(\w+)\s(\w+)', []);
  try
    if FLREInstance.ExtractAll('Word pair - Pair Words', MultiExtractions) then
    begin
      // you can use both to get the indexes
      //for i := 0 to Length(MultiExtractions) - 1 do
      for i := Low(MultiExtractions) to High(MultiExtractions) do
      begin
        //for j := 0 to Length(MultiExtractions[i]) - 1 do
        for j := Low(MultiExtractions[i]) to High(MultiExtractions[i]) do
        begin
          _WriteLnTab(MultiExtractions[i, j]);
        end;
      end;
    end;

    SetLength(MultiExtractions, 0);
  finally
    FLREInstance.Free;
  end;
end;

procedure NamedCapturingGroups;
const
  RegexPatternDate = '(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})';
  TestStringDate: String = '2019-10-24';
  RegexPattern = '(?<color>br\w+n).*?(?<animal>f\wx)';
  TestString: String = 'The quick brown fox jumps over the lazy dog';
var
  FLREInstance: TFLRE;
  Captures: TFLRECaptures;
  MultiCaptures: TFLREMultiCaptures;
  i, j: Integer;
  Dictionary: TDictionary<String, String>;
begin
  Dictionary := TDictionary<String, String>.Create;
  try
    FLREInstance := TFLRE.Create(RegexPatternDate,[]);
    try
      if FLREInstance.Match(TestStringDate, Captures, 1) then
      begin
        for i := Low(Captures) to High(Captures) do
        begin
          _WriteLnTab('Group: ' + FLREInstance.NamedGroups[i*2]);
          _WriteLnTab('Match: ' + Copy(TestStringDate, Captures[i].Start, Captures[i].Length));
          Dictionary.Add(FLREInstance.NamedGroups[i*2], Copy(TestStringDate, Captures[i].Start, Captures[i].Length));
        end;
      end;
    finally
      FLREInstance.Free;
    end;

    _WriteLnTabTab(Format('mm-yyyy is %s-%s', [Dictionary['month'], Dictionary['year']]));
  finally
    Dictionary.Free;
  end;

  _WriteLnTab('');

  FLREInstance := TFLRE.Create(RegexPattern,[]);
  try
    if FLREInstance.MatchAll(TestString, MultiCaptures, 1, 1) then
    begin
      for i := Low(MultiCaptures) to High(MultiCaptures) do
      begin
        for j := Low(MultiCaptures[i]) to High(MultiCaptures[i]) do
        begin
          _WriteLnTab('Group: ' + FLREInstance.NamedGroups[i + j * 2]);
          _WriteLnTab('Match: ' + Copy(TestString, MultiCaptures[i][j].Start, MultiCaptures[i][j].Length));
          _WriteLnTab('');
        end;
      end;
    end;
  finally
    FLREInstance.Free;
  end;
end;

procedure FindPosition;
const
  RegexPatternDate = '\W\d\d';
  TestStringDate: String = '2019-10-24';
  RegexPattern = 'f\w+\s*\w+ps';
  RegexPatternNoMatch = 'f(\s)*x';
  TestString: String = 'The quick brown fox jumps over the lazy dog';
var
  FLREInstance: TFLRE;
begin
  FLREInstance := TFLRE.Create(RegexPatternDate,[]);
  try
    _WriteLnTab('Match found at Position: ' + IntToStr(FLREInstance.Find(TestStringDate, 1)));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPattern,[]);
  try
    _WriteLnTab('Match found at Position: ' + IntToStr(FLREInstance.Find(TestString, 1)));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternNoMatch,[]);
  try
    _WriteLnTab('Match found at Position: ' + IntToStr(FLREInstance.Find(TestString, 1)));
  finally
    FLREInstance.Free;
  end;
end;

procedure Test;
const
  RegexPatternDate = '\W\d\d';
  RegexPatternDateFullMatch = '\d+-\d+-\d+';
  TestStringDate: String = '2019-10-24';
  RegexPattern = 'f\w+\s*\w+ps';
  RegexPatternNoMatch = 'f(\s)*x';
  TestString: String = 'The quick brown fox jumps over the lazy dog';
var
  FLREInstance: TFLRE;
begin
  FLREInstance := TFLRE.Create(RegexPatternDate,[]);
  try
    _WriteLnTab('Matches anchored?: ' + BoolToStr(FLREInstance.Test(TestStringDate, 1), True));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternDateFullMatch,[]);
  try
    _WriteLnTab('Matches anchored?: ' + BoolToStr(FLREInstance.Test(TestStringDate, 1), True));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPattern,[]);
  try
    _WriteLnTab('Matches anchored?: ' + BoolToStr(FLREInstance.Test(TestString, 1), True));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternNoMatch,[]);
  try
    _WriteLnTab('Matches anchored?: ' + BoolToStr(FLREInstance.Test(TestString, 1), True));
  finally
    FLREInstance.Free;
  end;


  FLREInstance := TFLRE.Create(RegexPatternDate,[]);
  try
    _WriteLnTab('Matches unanchored?: ' + BoolToStr(FLREInstance.TestAll(TestStringDate, 1), True));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPattern,[]);
  try
    _WriteLnTab('Matches unanchored?: ' + BoolToStr(FLREInstance.TestAll(TestString, 1), True));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternNoMatch,[]);
  try
    _WriteLnTab('Matches unanchored?: ' + BoolToStr(FLREInstance.TestAll(TestString, 1), True));
  finally
    FLREInstance.Free;
  end;
end;

procedure Replace;
const
  RegexPatternDate = '\W\d\d';
  RegexPatternDateFullMatch = '\d+-\d+-\d+';
  RegexPatternNumbers = '\d+';
  TestStringDate: String = '2019-10-24';
var
  FLREInstance: TFLRE;
begin
  FLREInstance := TFLRE.Create(RegexPatternDate,[]);
  try
    _WriteLnTab(FLREInstance.Replace(TestStringDate, '-12', 1, 1));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternDateFullMatch,[]);
  try
    _WriteLnTab(FLREInstance.Replace(TestStringDate, 'No date anymore', 1, 1));
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternNumbers,[]);
  try
    _WriteLnTab(FLREInstance.Replace(TestStringDate, '123', 1));

    _WriteLnTab(FLREInstance.RegularExpressionSource);
    _WriteLnTab(FLREInstance.DumpRegularExpression);
  finally
    FLREInstance.Free;
  end;
end;

procedure ConvertedExpressions;
const
  RegexPatternDate = '\W\d\d';
  RegexPatternDateFullMatch = '\d+-\d+-\d+';
  RegexPatternNumbers = '\d+';
var
  FLREInstance: TFLRE;
begin
  FLREInstance := TFLRE.Create(RegexPatternDate,[]);
  try
    _WriteLnTab(FLREInstance.RegularExpressionSource + '   ->   ' + FLREInstance.DumpRegularExpression);
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternDateFullMatch,[]);
  try
    _WriteLnTab(FLREInstance.RegularExpressionSource + '   ->   ' + FLREInstance.DumpRegularExpression);
  finally
    FLREInstance.Free;
  end;

  FLREInstance := TFLRE.Create(RegexPatternNumbers,[]);
  try
    _WriteLnTab(FLREInstance.RegularExpressionSource + '   ->   ' + FLREInstance.DumpRegularExpression);
  finally
    FLREInstance.Free;
  end;
end;

end.
