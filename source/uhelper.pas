unit uhelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function DetectFileEncoding(const FileName: string): TEncoding;
function GetLoggedInUserName: string;

implementation

function IsValidUTF8(const S: rawbytestring): boolean;
var
  i, Len, FollowBytes: integer;
  b: byte;
begin
  Result := True;
  i := 1;
  Len := Length(S);

  while i <= Len do
  begin
    b := byte(S[i]);

    if b < $80 then
      FollowBytes := 0
    else if (b and $E0) = $C0 then
      FollowBytes := 1
    else if (b and $F0) = $E0 then
      FollowBytes := 2
    else if (b and $F8) = $F0 then
      FollowBytes := 3
    else
    begin
      Result := False;
      Exit;
    end;

    if i + FollowBytes > Len then
    begin
      Result := False;
      Exit;
    end;

    while FollowBytes > 0 do
    begin
      Inc(i);
      b := byte(S[i]);
      if (b and $C0) <> $80 then
      begin
        Result := False;
        Exit;
      end;
      Dec(FollowBytes);
    end;

    Inc(i);
  end;
end;

function DetectFileEncoding(const FileName: string): TEncoding;
var
  FS: TFileStream;
  Buffer: array[0..2] of byte;
  BytesRead: integer;
  S: rawbytestring;
begin
  Result := TEncoding.ANSI; // default
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FS.Read(Buffer, SizeOf(Buffer));

    // UTF-8 BOM
    if (BytesRead >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and
      (Buffer[2] = $BF) then
    begin
      Result := TEncoding.UTF8;
      Exit;
    end;

    // UTF-16 BOM
    if (BytesRead >= 2) and (Buffer[0] = $FF) and (Buffer[1] = $FE) then
    begin
      Result := TEncoding.Unicode; // UTF-16 LE
      Exit;
    end
    else if (BytesRead >= 2) and (Buffer[0] = $FE) and (Buffer[1] = $FF) then
    begin
      Result := TEncoding.BigEndianUnicode; // UTF-16 BE
      Exit;
    end;

    // if no BOM -> read and check if valid UTF-8
    FS.Position := 0;
    SetLength(S, FS.Size);
    if FS.Size > 0 then
    begin
      FS.ReadBuffer(S[1], FS.Size);
      if IsValidUTF8(S) then
        Result := TEncoding.UTF8
      else
        Result := TEncoding.ANSI;
    end;
  finally
    FS.Free;
  end;
end;

function GetLoggedInUserName: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERNAME');
  {$ELSE} // Linux, macOS, etc.
  Result := GetEnvironmentVariable('USER');
  {$ENDIF}
end;

end.
