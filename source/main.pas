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
    actFont: TAction;
    actFullScreen: TAction;
    actAbout: TAction;
    actInsertDate: TAction;
    actInsertTime: TAction;
    actInstertDateTime: TAction;
    actInsertGUID: TAction;
    actInsertUserName: TAction;
    actLoadOptions: TAction;
    actSaveOptions: TAction;
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
    dlgFont: TFontDialog;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    dlgReplace: TReplaceDialog;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
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
    procedure actAboutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actFullScreenExecute(Sender: TObject);
    procedure actHandleParamsExecute(Sender: TObject);
    procedure actInsertDateExecute(Sender: TObject);
    procedure actInsertGUIDExecute(Sender: TObject);
    procedure actInsertTimeExecute(Sender: TObject);
    procedure actInsertUserNameExecute(Sender: TObject);
    procedure actInstertDateTimeExecute(Sender: TObject);
    procedure actLoadOptionsExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveOptionsExecute(Sender: TObject);
    procedure actShowInfoExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure tmrMainTimer(Sender: TObject);
    procedure WordWrapClick(Sender: TObject);
  private
    fFileName: string;
    fEncoding: TEncoding;
    fFoundPos: integer;
    fFullScreen: boolean;
    procedure LoadFile(const fn: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  uhelper,
  IniFiles,
  StrUtils;

resourcestring
  rsFilter = 'Text files (*.txt)|*.txt;*.TXT|All files (*.*)|*.*';
  rsStatusMsg = 'Lines: %d | %s';
  rsNoMoreResults = 'No more results';
  rsAbout = 'About';
  rsAboutInfo = '%s v%s'+#10+'© %s'+#10+'More Info: %s';

const
  keyLeft = 'window.left';
  keyTop = 'window.top';
  keyWidth = 'window.width';
  keyHeight = 'window.height';
  keyFontName = 'font.name';
  keyFontSize = 'font.size';

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //actDarkMode.Execute;
  actLoadOptions.Execute;
  memoMain.Font.Size := 12;
  fFullScreen := False;
  actNew.Execute;
  actShowInfo.Execute;
  actHandleParams.Execute;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  fFileName := FileNames[0];
  LoadFile(fFileName);
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
  fEncoding := DetectFileEncoding(fn);
  memoMain.Lines.LoadFromFile(fn, fEncoding);
  fFileName := fn;
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
begin
  Caption := format('%s - [ %s ]', [Application.Title, fFileName]);
  sbMain.SimpleText := format(rsStatusMsg, [memoMain.Lines.Count,
    fEncoding.EncodingName]);
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  memoMain.Undo;
end;

procedure TfrmMain.dlgFindFind(Sender: TObject);
begin
  with Sender as TFindDialog do
  begin
    fFoundPos := PosEx(unicodestring(FindText), unicodestring(memoMain.Lines.Text),
      fFoundPos + 1);
    if fFoundPos > 0 then
    begin
      memoMain.SelStart := fFoundPos - 1;
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
    fFoundPos := PosEx(unicodestring(FindText), unicodestring(memoMain.Lines.Text),
      fFoundPos + 1);
    if fFoundPos > 0 then
    begin
      memoMain.SelStart := fFoundPos - 1;
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

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  actSaveOptions.Execute;
  CloseAction := caFree;
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
      fFileName := od.FileName;
      LoadFile(fFileName);
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
      fFoundPos := 0
    else
      fFoundPos := memoMain.SelStart;
    Execute;
  end;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  memoMain.CutToClipboard;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  actSaveOptions.Execute;
  Close;
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
  with dlgFind do
  begin
    if frEntireScope in Options then  // search form the start
      fFoundPos := 0
    else
      fFoundPos := memoMain.SelStart;   // search form actual position
    Execute;
  end;
end;

procedure TfrmMain.actFontExecute(Sender: TObject);
begin
  dlgFont.Font := memoMain.Font;
  if dlgFont.Execute then memoMain.Font := dlgFont.Font;
end;

procedure TfrmMain.actFullScreenExecute(Sender: TObject);
begin
  if not fFullScreen then
  begin
    Self.WindowState := wsFullScreen;
    Self.Menu := nil;
    sbMain.Visible := False;
  end
  else
  begin
    Self.WindowState := wsNormal;
    Self.Menu := mnuMain;
    sbMain.Visible := True;
  end;
  fFullScreen := not fFullScreen;
end;

procedure TfrmMain.actHandleParamsExecute(Sender: TObject);
begin
  if ParamCount > 0 then
  begin
    if FileExists(ParamStr(1)) then
    begin
      fFileName := ParamStr(1);
      LoadFile(fFileName);
      actShowInfo.Execute;
    end;
  end;
end;

procedure TfrmMain.actInsertDateExecute(Sender: TObject);
begin
  memoMain.SelText := DateToStr(Now);
end;

procedure TfrmMain.actInsertGUIDExecute(Sender: TObject);
var
  guid: TGUID;
begin
  CreateGUID(guid);
  memoMain.SelText := GUIDToString(guid);
end;

procedure TfrmMain.actInsertTimeExecute(Sender: TObject);
begin
  memoMain.SelText := TimeToStr(Now);
end;

procedure TfrmMain.actInsertUserNameExecute(Sender: TObject);
begin
  memoMain.SelText := GetLoggedInUserName;
end;

procedure TfrmMain.actInstertDateTimeExecute(Sender: TObject);
begin
  memoMain.SelText := DateTimeToStr(Now);
end;

procedure TfrmMain.actLoadOptionsExecute(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(GetAppConfigFile(False));
  try
    Self.Left := ini.ReadInteger(Application.Title, keyLeft, 100);
    Self.Top := ini.ReadInteger(Application.Title, keyTop, 100);
    Self.Width := ini.ReadInteger(Application.Title, keyWidth, 640);
    Self.Height := ini.ReadInteger(Application.Title, keyHeight, 480);
    memoMain.Font.Name := ini.ReadString(Application.Title, keyFontName, 'default');
    memoMain.Font.Size := ini.ReadInteger(Application.Title, keyFontSize, 12);
  finally
    ini.Free;
  end;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  fFileName := '';
  memoMain.Clear;
  fEncoding := TEncoding.Default;
  actShowInfo.Execute;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  memoMain.CopyToClipboard;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
{$I litepad_version.inc}
begin
  MessageDlg(rsAbout, Format(rsAboutInfo, [AppLication.Title, APP_VERSION,
    'Zoltan Faludi', 'https://github.com/faludiz/LitePAD']), mtInformation, [mbOK], 0);
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);
  try
    sd.Filter := rsFilter;
    sd.Options := sd.Options + [ofOverwritePrompt];
    if fFileName <> '' then
    begin
      sd.InitialDir := ExtractFilePath(fFileName);
      sd.FileName := ExtractFileName(fFileName);
    end;
    if sd.Execute then
    begin
      fFileName := sd.FileName;
      memoMain.Lines.SaveToFile(fFileName, fEncoding);
      actShowInfo.Execute;
    end;
  finally
    sd.Free;
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if fFileName <> '' then memoMain.Lines.SaveToFile(fFileName)
  else
    actSaveAs.Execute;
end;

procedure TfrmMain.actSaveOptionsExecute(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(GetAppConfigFile(False));
  try
    ini.WriteInteger(Application.Title, keyLeft, Self.Left);
    ini.WriteInteger(Application.Title, keyTop, Self.Top);
    ini.WriteInteger(Application.Title, keyWidth, Self.Width);
    ini.WriteInteger(Application.Title, keyHeight, Self.Height);
    ini.WriteString(Application.Title, keyFontName, memoMain.Font.Name);
    ini.WriteInteger(Application.Title, keyFontSize, memoMain.Font.Size);
  finally
    ini.Free;
  end;
end;

end.
