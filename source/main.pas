unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus, ActnList, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actDarkMode: TAction;
    actHandleParams: TAction;
    actFind: TAction;
    actReplace: TAction;
    actNew: TAction;
    actUndo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actExit: TAction;
    actShowInfo: TAction;
    alMain: TActionList;
    dlgFind: TFindDialog;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    dlgReplace: TReplaceDialog;
    MenuItem13: TMenuItem;
    WordWrap: TMenuItem;
    Separator2: TMenuItem;
    tmrMain: TIdleTimer;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Separator1: TMenuItem;
    MenuItem5: TMenuItem;
    mnuMain: TMainMenu;
    memoMain: TMemo;
    sbMain: TStatusBar;
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actHandleParamsExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actShowInfoExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure tmrMainTimer(Sender: TObject);
    procedure WordWrapClick(Sender: TObject);
  private
    FileName: string;
    Encoding: TEncoding;
    FFoundPos: integer;
    procedure LoadFile(const fn: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uhelper,
  StrUtils;

resourcestring
  rsFilter = 'Text files (*.txt)|*.txt;*.TXT|All files (*.*)|*.*';
  rsStatusMsg = 'Lines: %d | %s';
  rsNoMoreResults = 'No more results';

  {$R *.lfm}

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  actDarkMode.Execute;
  memoMain.Font.Size := 12;
  actNew.Execute;
  actShowInfo.Execute;
  actHandleParams.Execute;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  FileName := FileNames[0];
  LoadFile(FileName);
  actShowInfo.Execute;
end;

procedure TfrmMain.tmrMainTimer(Sender: TObject);
begin
  actUndo.Enabled := memoMain.CanUndo;
  actShowInfo.Execute;
end;

procedure TfrmMain.WordWrapClick(Sender: TObject);
begin
  memoMain.WordWrap := (Sender as TMenuItem).Checked;
end;

procedure TfrmMain.LoadFile(const fn: string);
begin
  Encoding := DetectFileEncoding(fn);
  memoMain.Lines.LoadFromFile(fn, Encoding);
  FileName := fn;
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
begin
  Caption := format('%s - [ %s ]', [Application.Title, Filename]);
  sbMain.SimpleText := format(rsStatusMsg, [memoMain.Lines.Count,
    Encoding.EncodingName]);
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  memoMain.Undo;
end;

procedure TfrmMain.dlgFindFind(Sender: TObject);
begin
  with Sender as TFindDialog do
  begin
    FFoundPos := PosEx(unicodestring(FindText), unicodestring(memoMain.Lines.Text),
      FFoundPos + 1);
    if FFoundPos > 0 then
    begin
      memoMain.SelStart := FFoundPos - 1;
      memoMain.SelLength := Length(unicodestring(FindText));
      memoMain.SetFocus;
      // memoMain must be activated, otherwise the selection effect will not be displayed
    end
    else
      Beep();
  end;
end;

procedure TfrmMain.dlgReplaceFind(Sender: TObject);
begin
  with Sender as TReplaceDialog do
  begin
    FFoundPos := PosEx(unicodestring(FindText), unicodestring(memoMain.Lines.Text),
      FFoundPos + 1);
    if FFoundPos > 0 then
    begin
      memoMain.SelStart := FFoundPos - 1;
      memoMain.SelLength := Length(unicodestring(FindText));
      memoMain.SetFocus;
      // memoMain must be activated, otherwise the selection effect will not be displayed
    end
    else
      Beep();
  end;
end;

procedure TfrmMain.dlgReplaceReplace(Sender: TObject);
var
  StartPos, FoundPos: integer;
  SearchText, ReplaceText: string;
begin
  SearchText := dlgReplace.FindText;
  ReplaceText := dlgReplace.ReplaceText;

  // If there is no selected text, the part after the cursor is examined.
  StartPos := memoMain.SelStart + memoMain.SelLength;

  // Search
  FoundPos := PosEx(SearchText, memoMain.Text, StartPos + 1);

  if FoundPos > 0 then
  begin
    // Select tect
    memoMain.SelStart := FoundPos - 1;
    memoMain.SelLength := Length(SearchText);

    // Replace if the option allows
    if frReplace in dlgReplace.Options then
      memoMain.SelText := ReplaceText
    else if frReplaceAll in dlgReplace.Options then
      memoMain.Text := StringReplace(memoMain.Text, SearchText,
        ReplaceText, [rfReplaceAll]);
  end
  else
    ShowMessage(rsNoMoreResults);
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(Self);
  try
    od.Filter := rsFilter;
    if od.Execute then
    begin
      FileName := od.FileName;
      LoadFile(FileName);
      actShowInfo.Execute;
    end;
  finally
    od.Free;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  memoMain.PasteFromClipboard;
end;

procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  with dlgReplace do
  begin
    if frEntireScope in Options then
      FFoundPos := 0
    else
      FFoundPos := memoMain.SelStart;
    Execute;
  end;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  memoMain.CutToClipboard;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
  with dlgFind do
  begin
    if frEntireScope in Options then  // search form the start
      FFoundPos := 0
    else
      FFoundPos := memoMain.SelStart;   // search form actual position
    Execute;
  end;
end;

procedure TfrmMain.actHandleParamsExecute(Sender: TObject);
begin
  if ParamCount > 0 then
  begin
    if FileExists(ParamStr(1)) then
    begin
      FileName := ParamStr(1);
      LoadFile(FileName);
      actShowInfo.Execute;
    end;
  end;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  FileName := '';
  memoMain.Clear;
  Encoding := TEncoding.Default;
  actShowInfo.Execute;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  memoMain.CopyToClipboard;
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);
  try
    sd.Filter := rsFilter;
    sd.Options := sd.Options + [ofOverwritePrompt];
    if FileName <> '' then
    begin
      sd.InitialDir := ExtractFilePath(FileName);
      sd.FileName := ExtractFileName(FileName);
    end;
    if sd.Execute then
    begin
      FileName := sd.FileName;
      memoMain.Lines.SaveToFile(FileName, Encoding);
      actShowInfo.Execute;
    end;
  finally
    sd.Free;
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if FileName <> '' then memoMain.Lines.SaveToFile(FileName)
  else
    actSaveAs.Execute;
end;

end.
