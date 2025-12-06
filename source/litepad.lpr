program litepad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main,
  {$ifdef Windows}
    uDarkStyleParams,
    uMetaDarkStyle,
    uDarkStyleSchemes
  {$endif}
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='LitePAD';
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  {$ifdef Windows}
    PreferredAppMode:=pamDefault;
    ApplyMetaDarkStyle(DefaultDark);
  {$endif}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

