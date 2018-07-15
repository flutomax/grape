{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uHoleSizeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ColorBox,
  Spin, StdCtrls, uGlobal, uProfile;

type

  { TFrmHoleSize }

  TFrmHoleSize = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    cbColor: TColorBox;
    edScreenSize: TFloatSpinEdit;
    edRealSize: TFloatSpinEdit;
    lbScreenSize: TLabel;
    lbRealSize: TLabel;
    lbColor: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fScreenSize: double;
    procedure ReadSettings;
    procedure WriteSettings;
  public
    procedure SetParams(aItem: TSizeItem);
    procedure GetParams(aItem: TSizeItem);
  end;

var
  FrmHoleSize: TFrmHoleSize;

implementation

{$R *.lfm}

uses
  uMain, uProfileEditor;

{ TFrmHoleSize }

procedure TFrmHoleSize.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ShowHint:=Application.ShowHint;
  cbColor.Items.Clear;
  for i:=0 to 15 do
    cbColor.Items.AddObject(HOLES_CNAMES[i],TObject(PtrInt(HOLES_CLRS[i])));
end;

procedure TFrmHoleSize.FormHide(Sender: TObject);
begin
  WriteSettings;
end;

procedure TFrmHoleSize.FormShow(Sender: TObject);
begin
  if Application.Terminated then
    exit;
  ReadSettings;
end;

procedure TFrmHoleSize.SetParams(aItem: TSizeItem);
begin
  fScreenSize:=aItem.ScreenSize;
  edScreenSize.Value:=aItem.ScreenSize;
  edRealSize.Value:=aItem.RealSize;
  cbColor.Selected:=HOLES_CLRS[aItem.ColorIndex];
end;

procedure TFrmHoleSize.GetParams(aItem: TSizeItem);
begin
  aItem.ScreenSize:=edScreenSize.Value;
  aItem.RealSize:=edRealSize.Value;
  aItem.ColorIndex:=cbColor.ItemIndex;
end;

procedure TFrmHoleSize.ReadSettings;
var
  ini: TIniFileEx;
begin
  ini:=TIniFileEx.Create(FrmMain.IniFileName);
  try
    Left:=ini.ReadInteger(Name,'Left',Left);
    Top:=ini.ReadInteger(Name,'Top',Top);
  finally
    ini.Free;
  end;
end;

procedure TFrmHoleSize.WriteSettings;
var
  ini: TIniFileEx;
begin
  ini:=TIniFileEx.Create(FrmMain.IniFileName);
  try
    ini.WriteInteger(Name,'Left',Left);
    ini.WriteInteger(Name,'Top',Top);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmHoleSize.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (Modalresult=mrCancel) or (fScreenSize=edScreenSize.Value) then
    exit;
  if FrmProfile.HoleSizeExists(edScreenSize.Value) then begin
    CanClose:=false;
    MessageDlg('Warning',
      Format('Hole size with screen size %g is already exists, do not overwrite!',
      [edScreenSize.Value]),mtWarning,[mbOK],0);
    edScreenSize.SetFocus;
  end;
end;

end.

