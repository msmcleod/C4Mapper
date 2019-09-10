unit uStringCruncher;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

{ TStringCruncher }

TStringCruncher = class
protected
  class function MakeCrunchChar(c, prev, next : char) : char;
  class function GetCharPriority(c, prev, next : char) : integer;
  class function IsVowel(c : char) : boolean;
  class function IsConsonant(c : char) : boolean;
  class function IsDigit(c : char) : boolean;
  class function IsAlpha(c : char) : boolean;
  class function IsLower(c : char) : boolean;
  class function IsAlphaNumeric(c : char) : boolean;
public
  class function CrunchString(pszString : string; nBudget : integer) : string;
end;

implementation

{ TStringCruncher }

class function TStringCruncher.MakeCrunchChar(c, prev, next: char): char;
begin
  result := c;

  if (IsAlpha(c) and (not(IsAlphaNumeric(prev))) and IsLower(c)) then
  begin
    // Watch out for dB
    if ((c = 'd') and (next = 'B')) then
       result := c
    else
       result := UpCase(c);
  end;
end;

class function TStringCruncher.GetCharPriority(c, prev, next: char): integer;
begin
  if (IsDigit(c)) then
  begin
    result := 4;
    exit;
  end;

  if (((c = '-') or (c = '.')) and IsDigit(next)) then
  begin
    result := 4;
    exit;
  end;

  // Is it the first character of a word?
  if (IsAlpha(c) and (not(IsAlphaNumeric(prev)))) then
  begin
    result := 3;
    exit;
  end;

  if (IsConsonant(c)) then
  begin
    result := 2;
    exit;
  end;

  if (IsVowel(c)) then
  begin
    result := 1;
    exit;
  end;

  result := 0;
end;

class function TStringCruncher.IsVowel(c: char): boolean;
begin
  result := (c in ['A','E','I','O','U','a','e','i','o','u']);
end;

class function TStringCruncher.IsConsonant(c: char): boolean;
begin
  result := (IsAlpha(c) and (not(IsVowel(c))));
end;

class function TStringCruncher.IsDigit(c: char): boolean;
begin
  result := (c in ['0'..'9']);
end;

class function TStringCruncher.IsAlpha(c: char): boolean;
begin
  result := (c in ['A'..'Z','a'..'z']);
end;

class function TStringCruncher.IsLower(c: char): boolean;
begin
  result := (c in ['a'..'z']);
end;

class function TStringCruncher.IsAlphaNumeric(c: char): boolean;
begin
  result := (IsAlpha(c) or IsDigit(c));
end;

class function TStringCruncher.CrunchString(pszString: string; nBudget: integer): string;
var
  cbyString : integer;
  arPrioCounts : array [0..4] of integer;
  i : integer;
  prevChar,
  curChar,
  nextChar : char;
  ix,
  nprio,
  nBaseBudget,
  nBasePriority : integer;
begin
  pszString := trim(pszString);
  cbyString := length(pszString);
  result := '';

  // First pass: find the number of characters in each priority:

  if (cbyString <= nBudget) then
  begin
    result := pszString;
    exit; // string is ok as is
  end;

  // If you modify the maximum priority, make sure you change this
  for i := 0 to 4 do
    arPrioCounts[i] := 0;

  prevChar := #0;
  curChar := #0;
  nextChar := #0;

  for ix := 0 to cbyString-1 do
  begin
      curChar := pszString[ix + 1];
      nextChar := pszString[ix + 2];
      nPrio := GetCharPriority(curChar, prevChar, nextChar);
      inc(arPrioCounts[nPrio]);
      prevChar := curChar;
  end;

  // Triage by priority (find the lowest priority to include,
  // and the remaining budget)
  nBaseBudget := nBudget;

  for ix := 4 downto 0 do
  begin
    if (arPrioCounts[ix] <= nBaseBudget) then
       nBaseBudget := nBaseBudget - arPrioCounts[ix]
    else
       break;
  end;

  nBasePriority := ix;

  for ix := 0 to cbyString-1 do
  begin
    curChar := pszString[ix + 1];
    nextChar := pszString[ix + 2];

    nPrio := GetCharPriority(curChar, prevChar, nextChar);

    if (nPrio > nBasePriority) then
    begin
       result := result + MakeCrunchChar(curChar, prevChar, nextChar);
    end
    else if ((nPrio = nBasePriority) and (nBaseBudget > 0)) then
    begin
       result := result + MakeCrunchChar(curChar, prevChar, nextChar);
       dec(nBaseBudget);
    end;

    prevChar := curChar;
  end;
end;

end.

