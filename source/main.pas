unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus, ActnList, ExtCtrls, MD5;

type

  { TfrmMain }

  TfrmMain = class(TForm)
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
    actFontSizeUp: TAction;
    actFontSizeDown: TAction;
    actFontSizeDefault: TAction;
    actLinuxShortCut: TAction;
    actJump: TAction;
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
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
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
    procedure actFontSizeUpExecute(Sender: TObject);
    procedure actFontSizeDownExecute(Sender: TObject);
    procedure actFontSizeDefaultExecute(Sender: TObject);
    procedure actJumpExecute(Sender: TObject);
    procedure actLinuxShortCutExecute(Sender: TObject);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure tmrMainTimer(Sender: TObject);
    procedure WordWrapClick(Sender: TObject);
  private
    fFileName: string;
    fEncoding: TEncoding;
    fFoundPos: integer;
    fFullScreen: boolean;
    fFontSize: integer;
    fMD5: TMD5Digest;
    function HandleChanges: boolean;
    procedure LoadFile(const fn: string);
    function SaveFile: boolean;
    function Changed: boolean;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  about,
  uhelper,
  LclIntf,
  IniFiles,
  StrUtils;

resourcestring
  rsFilter = 'Text files (*.txt)|*.txt;*.TXT|All files (*.*)|*.*';
  rsStatusMsg = 'Lines: %d | Line: %d | Col: %d | Encoding: %s';
  rsNoMoreResults = 'No more results';
  rsJumpTo = 'Jump to line:';
  rsMsgSaveQuery = 'Do You want to save the changes?';
  rsShortCutDone = 'ShoertCut done';

const
  keyLeft = 'window.left';
  keyTop = 'window.top';
  keyWidth = 'window.width';
  keyHeight = 'window.height';
  keyFontName = 'font.name';
  keyFontSize = 'font.size';
  keyFontStyle = 'font.style';

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  actLoadOptions.Execute;
  fFullScreen := False;
  memoMain.Clear;
  fMD5 := MD5String(memoMain.Text);
  actNew.Execute;
  actShowInfo.Execute;
  actHandleParams.Execute;
  {$ifdef linux}
  actLinuxShortCut.Visible := True;
  {$endif}
  {$ifdef windows}
  actLinuxShortCut.Visible := False;
  {$endif}
end;

function TfrmMain.HandleChanges: boolean;
var
  mr: TModalResult;
begin
  Result := True;
  if Changed then
  begin
    mr := MessageDlg(Application.Title, rsMsgSaveQuery, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if mr = mrYes then
    begin
      Result := SaveFile;
    end;
    if mr = mrNo then
    begin
      Result := True;
    end;
    if mr = mrCancel then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  HandleChanges;
  memoMain.Clear;
  fFileName := '';
  fMD5 := MD5String(memoMain.Text);
  fEncoding := TEncoding.Default;
  actShowInfo.Execute;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
var
  od: TOpenDialog;
begin
  HandleChanges;
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

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
begin
  SaveFile;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if fFileName <> '' then memoMain.Lines.SaveToFile(fFileName)
  else
    actSaveAs.Execute;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if not HandleChanges then Exit;
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
  fMD5 := MD5String(memoMain.Text);
  fFileName := fn;
end;

function TfrmMain.SaveFile: boolean;
var
  sd: TSaveDialog;
begin
  Result := False;
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
      Result := True;
    end;
  finally
    sd.Free;
  end;
end;

function TfrmMain.Changed: boolean;
begin
  Result := True;
  if not Assigned(memoMain) then Exit;
  Result := not MD5Match(MD5String(memoMain.Text), fMD5);
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
var
  line, col: integer;
  changeFlag: string[1];
begin
  if Changed then changeFlag := '*'
  else
    changeFlag := '';
  Caption := format('%s - [ %s%s ]', [Application.Title, changeFlag, fFileName]);
  line := memoMain.CaretPos.Y + 1;
  col := memoMain.CaretPos.X + 1;
  sbMain.SimpleText := format(rsStatusMsg, [memoMain.Lines.Count,
    line, col, fEncoding.EncodingName]);
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

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := HandleChanges;
  actSaveOptIons.Execute;
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

procedure TfrmMain.actFontSizeUpExecute(Sender: TObject);
begin
  memoMain.Font.Size := memoMain.Font.Size + 1;
end;

procedure TfrmMain.actFontSizeDownExecute(Sender: TObject);
begin
  memoMain.Font.Size := memoMain.Font.Size - 1;
end;

procedure TfrmMain.actFontSizeDefaultExecute(Sender: TObject);
begin
  memoMain.Font.Size := fFontSize;
end;

procedure TfrmMain.actJumpExecute(Sender: TObject);
var
  tmp: string;
  jump: integer;
  Pos, i: integer;
begin
  tmp := InputBox(Application.Title, rsJumpTo, IntToStr(memoMain.CaretPos.Y + 1));
  if TryStrToInt(tmp, jump) then
  begin
    if (jump < 1) or (jump > memoMain.Lines.Count) then Exit;
    Pos := 0;
    for i := 0 to jump - 2 do
      Inc(Pos, Length(memoMain.Lines[i]) + string(LineEnding).Length);
    memoMain.SelStart := pos;
    memoMain.SelLength := 0;
    memoMain.SelText := memoMain.SelText;
    memoMain.SetFocus;
  end;
end;

procedure TfrmMain.actLinuxShortCutExecute(Sender: TObject);
var
  ini:tinifile;
  inifn:string;
  inipath:string;
  exefn:string;
  exepath:string;
  homedir:string;
const
   section='Desktop Entry';
begin
  homedir:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  inipath:=IncludeTrailingPathDelimiter(homedir + '.local/share/applications');
  forcedirectories(inipath);
  inifn:='litepad.desktop';
  exefn:=paramstr(0);
  exepath:=IncludeTrailingPathDelimiter(extractfilepath(exefn));
  ini:=tinifile.Create(inipath + inifn);
  ini.WriteString(section, 'Name','LitePAD');
  ini.WriteString(section, 'Comment', 'A Simple Text Editor');
  ini.WriteString(section, 'Terminal', 'false');
  ini.WriteString(section, 'Type', 'Application');
  ini.WriteString(section, 'Categories', 'Office, Utilities');
  ini.WriteString(section, 'Keywords', 'text;editor;');
  ini.WriteString(section, 'Path', exepath);
  ini.WriteString(section, 'Exec', exefn);
  ini.WriteString(section, 'Icon', exepath + 'litepad_96.png');
  ini.UpdateFile;
  ini.Free;
  ShowMessage(rsShortCutDone);
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  memoMain.CopyToClipboard;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
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
    ini.WriteInteger(Application.Title, keyFontStyle, integer(memoMain.Font.Style));
    ini.UpdateFile;
  finally
    ini.Free;
  end;
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
    memoMain.ParentFont := False;
    memoMain.Font.Name := ini.ReadString(Application.Title, keyFontName, 'default');
    fFontSize := ini.ReadInteger(Application.Title, keyFontSize, 12);
    memoMain.Font.Size := fFontSize;
    memoMain.Font.Style := TFontStyles(ini.ReadInteger(Application.Title,
      keyFontStyle, 0));
    memoMain.Refresh;
  finally
    ini.Free;
  end;
end;

end.
