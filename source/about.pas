unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    imgLogo: TImage;
    lblUrl: TLabel;
    lblVersion: TLabel;
    lblAppName: TLabel;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure lblUrlClick(Sender: TObject);
    procedure lblUrlMouseEnter(Sender: TObject);
    procedure lblUrlMouseLeave(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

const
  {$I litepad_version.inc}
  URL = 'https://github.com/faludiz/LitePAD';


implementation

uses
  LclIntf;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblAppName.Caption:= Application.Title;
  lblVersion.Caption:= 'v' + APP_VERSION;
  lblUrl.Caption:= URL;
end;

procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then Close;
end;

procedure TfrmAbout.lblUrlClick(Sender: TObject);
begin
  LclIntf.OpenURL(URL);
end;

procedure TfrmAbout.lblUrlMouseEnter(Sender: TObject);
begin
  lblUrl.Font.Underline:= True;
end;

procedure TfrmAbout.lblUrlMouseLeave(Sender: TObject);
begin
  lblUrl.Font.Underline:= False;
end;

end.

