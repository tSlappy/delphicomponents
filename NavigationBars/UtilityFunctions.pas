unit UtilityFunctions;

interface

uses
  Classes, SysUtils;

  // Basic String functions
  function String_IsNullOrEmpty(const Str: String): Boolean;
  function String_Format(const Str: String; Params: Array of const): String;
  function String_Substring(const text: String; index, count: Integer): String; // 0 based!
  function String_Contains(const text, search: String): Boolean;
  function String_IndexOf(const text, search: String; startPos: Integer = 1): Integer; // 0 based!
  function String_LastIndexOf(const text, search: String): Integer; // 0 based!

  function StringUtils_Compare_IC(const strLeft, strRight: String): Boolean;
  function StringUtils_Match_IC(const strLeft, strRight: String): Boolean;
  function StringUtils_Match_CS(const strLeft, strRight: String): Boolean;
  function StringUtils_Contains_IC(const Str, strToSearchFor: String): Boolean;
  function StringUtils_SplitString(const StringToSplit: String; const Delimiter: Char): TStrings;

implementation

function String_IsNullOrEmpty(const Str: String): Boolean;
begin
  if (Length(Str) <= 0) then
    Result := True
  else
    Result := False;
end;

function String_Format(const Str: String; Params: Array of const): String;
begin
{$IFDEF VER200}
  // 2009
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER210}
  // 2010
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER220}
  // XE
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER230}
  // XE2
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER240}
  // XE3
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER250}
  // XE4
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER260}
  // XE5
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER270}
  // XE6
  Result := Format(Str, Params);
{$ENDIF}

{$IFDEF VER280}
  // XE7
  Result := String.Format(Str, Params);
{$ENDIF}

{$IFDEF VER290}
  // XE8
  Result := String.Format(Str, Params);
{$ENDIF}

{$IFDEF VER300}
  // 10 Seattle
  Result := String.Format(Str, Params);
{$ENDIF}
end;

function String_Substring(const text: String; index, count: Integer): String;
begin
  // Converts from 0 - based searching to 1-based
  Result := Copy(text, index + 1, count);
end;

function String_Contains(const text, search: String): Boolean;
begin
  Result := StringUtils_Contains_IC(text, search);
end;

function String_IndexOf(const text, search: String; startPos: Integer = 1): Integer;
{$IF Defined(VER200) OR Defined(VER210) OR Defined(VER220) OR Defined(VER230)  }
var
  I: Integer;
{$IFEND}
begin
  // Converts from 1 - based searching to 0-based
{$IF Defined(VER200) OR Defined(VER210) OR Defined(VER220) OR Defined(VER230) }
  // Note: String_IndexOf for RS 2009 and 2010 works fine only for 1-Length strings
  if(Length(search) = 1) then
  begin
    Result := -1;
    for I := startPos to Length(text) - 1 do
    begin
      if(text[i] = search[1]) then
      begin
        Result := i - 1;
        break;
      end;
    end;
  end
  else
    Result := Pos(search, text) - 1;
{$ELSE}
  Result := Pos(search, text, startPos) - 1;
{$IFEND}
end;

function String_LastIndexOf(const text, search: String): Integer;
var
  lastIndex, lastRes: Integer;
begin
{$IF Defined(VER200) OR Defined(VER210) OR Defined(VER220) OR Defined(VER230)  }
  // Note: String_LastIndexOf for RS 2009 and 2010 works fine only for 1-Length strings
  if(Length(search) = 1) then
  begin
    Result := -1;
    for lastIndex := Length(text) downto 1 do
    begin
      if(text[lastIndex] = search[1]) then
      begin
        Result := lastIndex - 1;
        break;
      end;
    end;
  end
  else
    Result := Pos(search, text) - 1;
{$ELSE}
  lastIndex := 1;
  while(lastIndex > 0) do
  begin
    lastRes := lastIndex;
    lastIndex := Pos(search, text, lastIndex);
  end;
  Result := lastRes - 1;
{$IFEND}
end;


function StringUtils_Compare_IC(const strLeft, strRight: String): Boolean;
begin
  if(CompareText(strLeft, strRight) = 0) then
    Result:= True
  else
    Result := False;
end;

function StringUtils_Match_IC(const strLeft, strRight: String): Boolean;
begin
  //!!! Upravit - preco?
  if(CompareText(strLeft, strRight) = 0) then
    Result:= True
  else
    Result := False;
end;

function StringUtils_Match_CS(const strLeft, strRight: String): Boolean;
begin
  //!!! Upravit - aby fungovalo ako CS
  if(CompareStr(strLeft, strRight) = 0) then
    Result:= True
  else
    Result := False;
end;

function StringUtils_Contains_IC(const Str, strToSearchFor: String): Boolean;
begin
  if(AnsiStrPos(Pchar(UpperCase(Str)), PChar(UpperCase(strToSearchFor))) = nil) then
    Result := False
  else
    Result := True;
end;

function StringUtils_SplitString(const StringToSplit: String; const Delimiter: Char): TStrings;
var
  ListOfStrings: TStrings;
begin
  try
     // http://stackoverflow.com/questions/2625707/split-a-string-into-an-array-of-strings-based-on-a-delimiter
     ListOfStrings := TStringList.Create;
     ListOfStrings.Clear;
     ListOfStrings.Delimiter       := Delimiter;
     ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
     ListOfStrings.DelimitedText   := StringToSplit;

     Result := ListOfStrings;
  except

  end;
end;

end.
