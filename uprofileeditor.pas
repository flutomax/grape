{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uProfileEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, ExtCtrls, ActnList, uGlobal, uProfile;

type

  { TFrmProfile }

  TFrmProfile = class(TForm)
    bntMaskColor: TColorButton;
    cbHolesShape: TComboBox;
    ckIgnoreMin: TCheckBox;
    ckUseColorMask: TCheckBox;
    cmdUseHoleSet: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ckInverted: TCheckBox;
    cmdHsDelete: TAction;
    cmdHsEdit: TAction;
    cmdHsAdd: TAction;
    alOptions: TActionList;
    CheckBox1: TCheckBox;
    edHolesDistance: TFloatSpinEdit;
    edMaxHolesSize: TFloatSpinEdit;
    edMinHolesSize: TFloatSpinEdit;
    ilLv: TImageList;
    lbGamma: TLabel;
    lbHolesDistance: TLabel;
    lbHolesSize: TLabel;
    lbHolesSize1: TLabel;
    lbMaskColor: TLabel;
    lbMinHolesSize: TLabel;
    lvHolesSet: TListView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tbGamma: TTrackBar;
    procedure bntMaskColorColorChanged(Sender: TObject);
    procedure cbHolesShapeChange(Sender: TObject);
    procedure ckIgnoreMinChange(Sender: TObject);
    procedure ckInvertedChange(Sender: TObject);
    procedure ckUseColorMaskChange(Sender: TObject);
    procedure cmdHsAddExecute(Sender: TObject);
    procedure cmdHsAddUpdate(Sender: TObject);
    procedure cmdHsDeleteExecute(Sender: TObject);
    procedure cmdHsEditExecute(Sender: TObject);
    procedure cmdHsEditUpdate(Sender: TObject);
    procedure cmdUseHoleSetExecute(Sender: TObject);
    procedure cmdUseHoleSetUpdate(Sender: TObject);
    procedure edHolesDistanceChange(Sender: TObject);
    procedure edMaxHolesSizeChange(Sender: TObject);
    procedure edMinHolesSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvHolesSetDblClick(Sender: TObject);
    procedure lvHolesSetItemChecked(Sender: TObject; Item: TListItem);
    procedure tbGammaChange(Sender: TObject);
  private
    procedure UpdateMaskColorMode;
    function EditHoleSize(aItem: TSizeItem): Boolean;
    procedure ReadSettings;
    procedure WriteSettings;
  public
    procedure UpdateProfile;
    function HoleSizeExists(const aScreenSize: Double): Boolean;
  end;

var
  FrmProfile: TFrmProfile;

implementation

{$R *.lfm}

uses
  uMain, uHoleSizeEditor;

{ TFrmProfile }

procedure TFrmProfile.FormCreate(Sender: TObject);
var
  i: Integer;
  bmp: TBitmap;
begin
  ShowHint:=Application.ShowHint;
  bmp:=TBitmap.Create;
  try
    bmp.SetSize(16,16);
    for i:=0 to 15 do
      with bmp.Canvas do begin
        Brush.Color:=HOLES_CLRS[i];
        Pen.Color:=clBlack;
        Rectangle(ClipRect);
        ilLv.AddMasked(bmp,$000001);
      end;
  finally
    bmp.Free;
  end;
  {$ifdef Windows}
  tbGamma.TickStyle:=tsAuto;
  {$endif}
end;

procedure TFrmProfile.FormHide(Sender: TObject);
begin
  WriteSettings;
end;

procedure TFrmProfile.FormShow(Sender: TObject);
begin
  if Application.Terminated then
    exit;
  ReadSettings;
  UpdateMaskColorMode;
  UpdateProfile;
  ShowHint:=Application.ShowHint;
end;

procedure TFrmProfile.ReadSettings;
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

procedure TFrmProfile.WriteSettings;
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

procedure TFrmProfile.lvHolesSetDblClick(Sender: TObject);
begin
  cmdHsEdit.Execute;
end;

procedure TFrmProfile.lvHolesSetItemChecked(Sender: TObject; Item: TListItem);
begin
  FrmMain.Profile.Sizes[Item.Index].Enabled:=Item.Checked;
end;

function TFrmProfile.HoleSizeExists(const aScreenSize: Double): Boolean;
var
  i: integer;
begin
  result:=false;
  with FrmMain.Profile do begin
    for i:=0 to SizesCount-1 do
      if Sizes[i].ScreenSize=aScreenSize then
        Exit(true);
  end;
end;

function TFrmProfile.EditHoleSize(aItem: TSizeItem): Boolean;
begin
  with FrmHoleSize do begin
    SetParams(aItem);
    result:=ShowModal=mrOK;
    if not result then
      exit;
    GetParams(aItem);
  end;
end;

procedure TFrmProfile.UpdateMaskColorMode;
begin
  bntMaskColor.Enabled:=ckUseColorMask.Checked;
  lbMaskColor.Enabled:=bntMaskColor.Enabled;
end;

procedure TFrmProfile.UpdateProfile;
var
  i: integer;
  item: TSizeItem;
  lst: TListItem;
begin
  with FrmMain.Profile do begin
    cbHolesShape.ItemIndex:=Ord(Shape);
    edMaxHolesSize.Value:=MaxSize;
    edMinHolesSize.Value:=MinSize;
    edHolesDistance.Value:=Distance;
    ckIgnoreMin.Checked:=IgnoreMin;
    ckInverted.Checked:=Inverted;
    tbGamma.Position:=Round(Gamma*100);
    lbGamma.Caption:='Gamma: '+FloatToStr(Gamma);
    ckUseColorMask.Checked:=UseColorMask;
    bntMaskColor.ButtonColor:=MaskColor;
    cmdUseHoleSet.Checked:=UseHoleSet;
    lvHolesSet.Items.BeginUpdate;
    try
      lvHolesSet.Items.Clear;
      for i:=0 to SizesCount-1 do begin
        item:=Sizes[i];
        lst:=lvHolesSet.Items.Add;
        lst.Caption:=IntToStr(i+1);
        lst.SubItems.Add(FloatToStr(item.ScreenSize));
        lst.SubItems.Add(FloatToStr(item.RealSize));
        lst.Checked:=item.Enabled;
        lst.ImageIndex:=item.ColorIndex;
      end;
    finally
      lvHolesSet.Items.EndUpdate;
    end;
  end;
  UpdateMaskColorMode;
end;

procedure TFrmProfile.cmdHsAddExecute(Sender: TObject);
var
  item: TSizeItem;
begin
  item:=FrmMain.Profile.AddSize;
  item.ColorIndex:=lvHolesSet.Items.Count mod 16;
  if not EditHoleSize(item) then begin
    FrmMain.Profile.RemoveSize(item);
    exit;
  end;
  UpdateProfile;
end;

procedure TFrmProfile.cmdHsAddUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=cmdUseHoleSet.Checked;
end;

procedure TFrmProfile.cmdHsDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Confirm','To delete the selected hole size from the list?',
    mtConfirmation,mbYesNo,0)<>mrYes then
      exit;
  FrmMain.Profile.DeleteSize(lvHolesSet.ItemIndex);
  UpdateProfile;
end;

procedure TFrmProfile.cmdHsEditExecute(Sender: TObject);
var
  item: TSizeItem;
begin
  if not EditHoleSize(FrmMain.Profile.Sizes[lvHolesSet.ItemIndex]) then
    exit;
  UpdateProfile;
end;

procedure TFrmProfile.cmdHsEditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=cmdUseHoleSet.Checked and Assigned(lvHolesSet.Selected);
end;

procedure TFrmProfile.cmdUseHoleSetExecute(Sender: TObject);
begin
  FrmMain.Profile.UseHoleSet:=cmdUseHoleSet.Checked;
end;

procedure TFrmProfile.cmdUseHoleSetUpdate(Sender: TObject);
begin
  lvHolesSet.Enabled:=cmdUseHoleSet.Checked;
end;

procedure TFrmProfile.ckInvertedChange(Sender: TObject);
begin
  FrmMain.Profile.Inverted:=ckInverted.Checked;
end;

procedure TFrmProfile.tbGammaChange(Sender: TObject);
begin
  with FrmMain.Profile do begin
    Gamma:=tbGamma.Position*0.01;
    lbGamma.Caption:='Gamma: '+FloatToStr(Gamma);
  end;
end;

procedure TFrmProfile.cbHolesShapeChange(Sender: TObject);
begin
  FrmMain.Profile.Shape:=THoleShape(cbHolesShape.ItemIndex);
end;

procedure TFrmProfile.edMaxHolesSizeChange(Sender: TObject);
begin
  FrmMain.Profile.MaxSize:=edMaxHolesSize.Value;
end;

procedure TFrmProfile.edMinHolesSizeChange(Sender: TObject);
begin
  FrmMain.Profile.MinSize:=edMinHolesSize.Value;
end;

procedure TFrmProfile.edHolesDistanceChange(Sender: TObject);
begin
  FrmMain.Profile.Distance:=edHolesDistance.Value;
end;

procedure TFrmProfile.ckIgnoreMinChange(Sender: TObject);
begin
  FrmMain.Profile.IgnoreMin:=ckIgnoreMin.Checked;
end;

procedure TFrmProfile.bntMaskColorColorChanged(Sender: TObject);
begin
  FrmMain.Profile.MaskColor:=bntMaskColor.ButtonColor;
end;

procedure TFrmProfile.ckUseColorMaskChange(Sender: TObject);
begin
  FrmMain.Profile.UseColorMask:=ckUseColorMask.Checked;
  UpdateMaskColorMode;
end;

end.

