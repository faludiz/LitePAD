program update_version;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DOM, XMLRead;

function FindNodeRecursive(ANode: TDOMNode; const AName: DOMString): TDOMNode;
var
  i: Integer;
  c: TDOMNode;
begin
  Result := nil;
  if (ANode = nil) then Exit;
  if SameText(UTF8Encode(ANode.NodeName), UTF8Encode(AName)) then
    Exit(ANode);
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    c := ANode.ChildNodes[i];
    Result := FindNodeRecursive(c, AName);
    if Assigned(Result) then Exit;
  end;
end;

function GetAttrOrDefault(ANode: TDOMNode; const AttrName, DefVal: string): string;
var
  Attr: TDOMNode;
begin
  Result := DefVal;
  if (ANode <> nil) and ANode.HasAttributes then
  begin
    Attr := ANode.Attributes.GetNamedItem(AttrName);
    if Assigned(Attr) then
      Result := UTF8Encode(Attr.NodeValue);
  end;
end;

var
  LpiFile, OutFile: string;
  Doc: TXMLDocument;
  VersionInfo, N: TDOMNode;
  Major, Minor, Revision, Build: string;
  F: TextFile;
begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' <projekt.lpi>');
    Halt(1);
  end;

  LpiFile := ParamStr(1);
  if not FileExists(LpiFile) then
  begin
    Writeln('Error! Unable to find file: ', LpiFile);
    Halt(1);
  end;

  try
    ReadXMLFile(Doc, LpiFile);
  except
    on E: Exception do
    begin
      Writeln('XML reading error: ', E.Message);
      Halt(1);
    end;
  end;

  try
    VersionInfo := FindNodeRecursive(Doc.DocumentElement, 'VersionInfo');
    if not Assigned(VersionInfo) then
    begin
      Writeln('Error: Not found <VersionInfo> in the .lpi file.');
      Halt(1);
    end;

    Major := '0';
    Minor := '0';
    Revision := '0';
    Build := '0';

    N := FindNodeRecursive(VersionInfo, 'MajorVersionNr');
    Major := GetAttrOrDefault(N, 'Value', '0');

    N := FindNodeRecursive(VersionInfo, 'MinorVersionNr');
    Minor := GetAttrOrDefault(N, 'Value', '0');

    N := FindNodeRecursive(VersionInfo, 'RevisionNr');
    Revision := GetAttrOrDefault(N, 'Value', '0'); 

    N := FindNodeRecursive(VersionInfo, 'BuildNr');
    Build := GetAttrOrDefault(N, 'Value', '0');

    OutFile := ChangeFileExt(LpiFile, '') + '_version.inc';
    AssignFile(F, OutFile);
    Rewrite(F);
    try
      Writeln(F, 'APP_VERSION = ''', Major, '.', Minor, '.', Revision, '.', Build, ''';');
    finally
      CloseFile(F);
    end;

    Writeln('Done ', OutFile, ' Created: ', Major, '.', Minor, '.', Revision, '.', Build);
  finally
    Doc.Free;
  end;
end.
