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
    actUndo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actExit: TAction;
    actSetCaption: TAction;
    alMain: TActionList;
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
    procedure actHandleParamsExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSetCaptionExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure tmrMainTimer(Sender: TObject);
    procedure WordWrapClick(Sender: TObject);
  private
    FileName: string;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

resourcestring
  rsFilter = 'Text files (*.txt)|*.txt;*.TXT|All files (*.*)|*.*';

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  actDarkMode.Execute;
  memoMain.Font.Size:= 12;
  memoMain.Text:= '';
  FileName:= '';
  actSetCaption.Execute;
  actHandleParams.Execute;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  FileName:= FileNames[0];
  memoMain.Lines.LoadFromFile(FileName);
  actSetCaption.Execute;
end;

procedure TfrmMain.tmrMainTimer(Sender: TObject);
begin
  actUndo.Enabled:= memoMain.CanUndo;
  sbMain.SimpleText:= format('Lines: %d', [memoMain.Lines.Count]);
end;

procedure TfrmMain.WordWrapClick(Sender: TObject);
begin
  memoMain.WordWrap:= (Sender as TMenuItem).Checked;
end;

procedure TfrmMain.actSetCaptionExecute(Sender: TObject);
begin
  if FileName <> '' then Caption:= format('%s - [ %s ]', [Application.Title, Filename]);
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  memoMain.Undo;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
var
  od: TOpenDialog;
begin
  od:= TOpenDialog.Create(Self);
  try
    od.Filter:= rsFilter;
    if od.Execute then begin
      FileName:= od.FileName;
      memoMain.Lines.LoadFromFile(FileName);
      actSetCaption.Execute;
    end;
  finally
    od.Free;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  memoMain.PasteFromClipboard;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  memoMain.CutToClipboard;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actHandleParamsExecute(Sender: TObject);
begin
  if ParamCount > 0 then begin
    if FileExists(ParamStr(1)) then begin
      FileName:=  ParamStr(1);
      memoMain.Lines.LoadFromFile(FileName);
      actSetCaption.Execute;
    end;
  end;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  memoMain.CopyToClipboard;
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd:= TSaveDialog.Create(Self);
  try
    sd.Filter:= rsFilter;
    sd.Options:= sd.Options + [ofOverwritePrompt];
    if FileName <> '' then begin
      sd.InitialDir:= ExtractFilePath(FileName);
      sd.FileName:= ExtractFileName(FileName);
    end;
    if sd.Execute then begin
      FileName:= sd.FileName;
      memoMain.Lines.SaveToFile(FileName);
      actSetCaption.Execute;
    end;
  finally
    sd.Free;
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if FileName <> '' then memoMain.Lines.SaveToFile(FileName) else actSaveAs.Execute;
end;

end.

