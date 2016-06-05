unit Utils;

interface

uses
  {$IFDEF VER210}
  SysUtils, Classes;
  {$ELSE}
  System.SysUtils, System.Classes;
  {$ENDIF}

function printData(data: PByte; len: Integer) : String;

implementation

function printData(data: PByte; len: Integer) : String;

  function fillHex(data, digits: Integer) : String; inline;
  begin
    Result:= LowerCase(IntToHex(data, digits), TLocaleOptions.loInvariantLocale);
  end;

var
  sb: TStringBuilder;
  counter, i, charpoint, a, t1, rest: Integer;
begin
  sb:= nil;
  try
    sb:= TStringBuilder.Create;

    counter:= 0;

    for i:= 0 to len - 1 do
    begin
      if counter mod 16 = 0 then
        sb.append(fillHex(i, 4) + ': ');

      sb.append(fillHex(data[i] and $ff, 2) + ' ');
      Inc(counter);
      if counter = 16 then
      begin
        sb.append('   ');
        charpoint:= i - 15;
        for a:= 0 to 16 - 1 do
        begin
          t1:= data[charpoint];
          Inc(charpoint);
          if (t1 > $1f) and (t1 < $80) then
            sb.append(Char(t1))
          else
            sb.append('.');
        end;
        sb.append(sLineBreak);
        counter:= 0;
      end;
    end;

    rest:= len mod 16;
    if rest > 0 then
    begin
      for i:= 0 to 17 - rest - 1 do
        sb.append('   ');

      charpoint:= len - rest;
      for a:= 0 to rest - 1 do
      begin
        t1:= data[charpoint];
        Inc(charpoint);
        if (t1 > $1f) and (t1 < $80) then
          sb.append(char(t1))
        else
          sb.append('.');
      end;
      sb.append(sLineBreak);
    end;

    Result:= sb.toString();
  finally
    FreeAndNil(sb);
  end;
end;

end.
