{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LMessages, ExtDlgs, ActnList, Menus, FPImage, GraphType, IntfGraphics,
  ComCtrls, StdCtrls, agg_fpimage, Agg_LCL, uGlobal, uProfile;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    cmdHomepage: TAction;
    cmdOutputImageCopy: TAction;
    cmdAutoloadLast: TAction;
    cmdAutoRefresh: TAction;
    cmdShowHints: TAction;
    cmdAbout: TAction;
    cmdSaveProfileAs: TAction;
    cmdSaveProfile: TAction;
    cmdFileOpenProfile: TAction;
    apMain: TApplicationProperties;
    cmdEditProfile: TAction;
    cmdRun: TAction;
    cmdExportVector: TAction;
    cmdFileOpenImage: TAction;
    cmdFileExit: TAction;
    alMain: TActionList;
    dlgOpenImage: TOpenPictureDialog;
    ImgInput: TImage;
    ImgOutput: TImage;
    ImLarge: TImageList;
    ImLargeD: TImageList;
    lmSmall: TImageList;
    mnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    dlgExportVector: TSaveDialog;
    MenuItem9: TMenuItem;
    dlgOpenProfile: TOpenDialog;
    pnInput: TPanel;
    pnInputImg: TPanel;
    pnMain: TPanel;
    pnOutput: TPanel;
    pnOutputImg: TPanel;
    dlgSaveProfile: TSaveDialog;
    sbInput: TScrollBox;
    sbMain: TStatusBar;
    sbOutput: TScrollBox;
    SplitterImg: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure apMainHint(Sender: TObject);
    procedure cmdAboutExecute(Sender: TObject);
    procedure cmdAutoRefreshExecute(Sender: TObject);
    procedure cmdFileExitExecute(Sender: TObject);
    procedure cmdFileOpenImageExecute(Sender: TObject);
    procedure cmdFileOpenProfileExecute(Sender: TObject);
    procedure cmdEditProfileExecute(Sender: TObject);
    procedure cmdHomepageExecute(Sender: TObject);
    procedure cmdOutputImageCopyExecute(Sender: TObject);
    procedure cmdOutputImageCopyUpdate(Sender: TObject);
    procedure cmdRunExecute(Sender: TObject);
    procedure cmdRunUpdate(Sender: TObject);
    procedure cmdExportVectorExecute(Sender: TObject);
    procedure cmdExportVectorUpdate(Sender: TObject);
    procedure cmdSaveProfileAsExecute(Sender: TObject);
    procedure cmdSaveProfileExecute(Sender: TObject);
    procedure cmdSaveProfileUpdate(Sender: TObject);
    procedure cmdShowHintsExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbInputResize(Sender: TObject);
    procedure SplitterImgMoved(Sender: TObject);
  private
    fHoles: TList;
    fProfile: TProfile;
    fMask: array of array of Boolean;
    fImage: TLazIntfImage;
    fAgg: TAggLCLCanvas;
    fIniFileName: string;
    fImgFileName: string;
    fLastFileName: string;
    fProfilesDir: string;
    fImagesDir: string;
    fVectorsDir: string;
    fLeftWidth: Double;
    fTotalWidth: Integer;
    function CreckProfileSaved: Boolean;
    function GetSquareIntensity(const a_x, a_y: Integer): Double;
    procedure ConvertToGrayScale;
    procedure Process;
    procedure ReadSettings;
    procedure WriteSettings;
    procedure ClearHoles;
    procedure MakeColorMask;
    procedure SetLeftWidth(const Value: Double);
    procedure UpdateLeftWidth;
    procedure UpdateProfile;
    procedure LoadProfile(const aFileName: string);
    procedure ProfileChange(Sender: TObject);
    procedure UpdateTitle;
    procedure SysResizing(const Cmd: PtrInt);
    procedure WMSysCommand(var Msg: TLMSysCommand); message LM_SYSCOMMAND;
  public
    property LeftWidth: Double read fLeftWidth write SetLeftWidth;
    property IniFileName: string read fIniFileName;
    property Profile: TProfile read fProfile;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  LCLIntf, Math, IniFiles, Clipbrd, uProfileEditor, uDxfWriter, uAbout, uMisc;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fProfile:=TProfile.Create;
  fProfile.OnChange:=@ProfileChange;
  fIniFileName:=ProgramDirectory+'options.ini';
  fHoles:=TList.Create;
  fImage:=TLazIntfImage.Create(0,0);
  fAgg:=nil;
  fLeftWidth:=0.5;
  fTotalWidth:=-1;
  MakeDisabledImageList(ImLarge,ImLargeD);
  ShowHint:=Application.ShowHint;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ClearHoles;
  WriteSettings;
  fMask:=nil;
  fHoles.Free;
  fImage.Free;
  fAgg.Free;
  fProfile.Free;
end;

procedure TFrmMain.FormResize(Sender: TObject);
begin
  UpdateLeftWidth;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  ReadSettings;
  if Paramcount>0 then
    LoadProfile(Paramstr(1))
  else
    if cmdAutoloadLast.Checked and FileExists(fLastFileName) then
      LoadProfile(fLastFileName);
  UpdateTitle;
end;

procedure TFrmMain.sbInputResize(Sender: TObject);
var
  img: TImage;
  sb: TScrollBox;
  aLeft,aTop,
  aWidth,aHeight: Integer;
begin
  if not (Sender is TScrollBox) then
    exit;
  sb:=Sender as TScrollBox;
  img:=sb.Controls[0] as TImage;
  Assert(img<>nil);
  if img.Picture.Bitmap.Empty then
    exit;
  aWidth:=img.Picture.Bitmap.Width;
  aHeight:=img.Picture.Bitmap.Height;
  aLeft:=Max(0,(sb.ClientWidth-aWidth) div 2);
  aTop:=Max(0,(sb.ClientHeight-aHeight) div 2);
  img.SetBounds(aLeft,aTop,aWidth,aHeight);
end;

procedure TFrmMain.SplitterImgMoved(Sender: TObject);
begin
  fLeftWidth:=pnInput.Width/(pnMain.ClientWidth-SplitterImg.Width);
end;

procedure TFrmMain.SetLeftWidth(const Value: Double);
begin
  fLeftWidth:=Value;
  UpdateLeftWidth;
end;

procedure TFrmMain.UpdateLeftWidth;
begin
  pnInput.Width:=Abs(Round(fLeftWidth*pnMain.ClientWidth)-SplitterImg.Width);
end;

procedure TFrmMain.SysResizing(const Cmd: PtrInt);
begin
  if (Cmd=SC_MAXIMIZE) or ((Cmd=SC_DEFAULT)
    and (WindowState<>wsMaximized)) then
      fTotalWidth:=pnInput.Width+pnOutput.Width
  else
    if ((Cmd=SC_RESTORE) or ((Cmd=SC_DEFAULT)
      and (WindowState=wsMaximized))) and (fTotalWidth>=0) then begin
        pnInput.Width:=Round(fLeftWidth*fTotalWidth);
        fTotalWidth:=-1;
    end;
end;

procedure TFrmMain.WMSysCommand(var Msg: TLMSysCommand);
var
  Cmd: PtrInt;
begin
  Cmd:=Msg.CmdType and $FFF0;
  case Cmd of
    SC_RESTORE,SC_MAXIMIZE,SC_MINIMIZE,SC_DEFAULT: SysResizing(Cmd);
  end;
  inherited;
end;

procedure TFrmMain.ReadSettings;
var
  ini: TIniFileEx;
begin
  ini:=TIniFileEx.Create(fIniFileName);
  try
    cmdShowHints.Checked:=ini.ReadBool('Common','ShowHint',true);
    cmdAutoRefresh.Checked:=ini.ReadBool('Common','AutoUpdate',true);
    cmdAutoloadLast.Checked:=ini.ReadBool('Common','AutoloadLast',true);
    fLastFileName:=ini.ReadString('Common','LastProfile','');
    Left:=ini.ReadInteger(Name,'Left',Left);
    Top:=ini.ReadInteger(Name,'Top',Top);
    Width:=ini.ReadInteger(Name,'Width',Width);
    Height:=ini.ReadInteger(Name,'Height',Height);
    WindowState:=TWindowState(ini.ReadInteger(Name,'WindowState',Ord(wsNormal)));
    LeftWidth:=ini.ReadFloat(Name,'LeftWidth',LeftWidth);
    fProfilesDir:=ini.ReadString('Directories','Profiles','');
    fImagesDir:=ini.ReadString('Directories','Images','');
    fVectorsDir:=ini.ReadString('Directories','Vectors','');
  finally
    ini.Free;
  end;
  cmdShowHintsExecute(nil);
end;

procedure TFrmMain.WriteSettings;
var
  ini: TIniFileEx;
begin
  ini:=TIniFileEx.Create(fIniFileName);
  try
    ini.WriteBool('Common','ShowHint',cmdShowHints.Checked);
    ini.WriteBool('Common','AutoUpdate',cmdAutoRefresh.Checked);
    ini.WriteBool('Common','AutoloadLast',cmdAutoloadLast.Checked);
    ini.WriteString('Common','LastProfile',fProfile.FileName);
    if WindowState in [wsNormal, wsMinimized] then begin
      ini.WriteInteger(Name,'Left',Left);
      ini.WriteInteger(Name,'Top',Top);
      ini.WriteInteger(Name,'Width',Width);
      ini.WriteInteger(Name,'Height',Height);
    end else begin
      ini.WriteInteger(Name,'Left',RestoredLeft);
      ini.WriteInteger(Name,'Top',RestoredTop);
      ini.WriteInteger(Name,'Width',RestoredWidth);
      ini.WriteInteger(Name,'Height',RestoredHeight);
    end;
    ini.WriteInteger(Name,'WindowState',Ord(WindowState));
    ini.WriteFloat(Name,'LeftWidth',LeftWidth);
    ini.WriteString('Directories','Profiles',fProfilesDir);
    ini.WriteString('Directories','Images',fImagesDir);
    ini.WriteString('Directories','Vectors',fVectorsDir);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.cmdShowHintsExecute(Sender: TObject);
begin
  Application.ShowHint:=cmdShowHints.Checked;
  ShowHint:=Application.ShowHint;
end;


function TFrmMain.CreckProfileSaved: Boolean;
begin
  result:=true;
  if fProfile.Changed then
    case MessageDlg('Grape',
    Format('Do you want to save changes in profile "%s"?',
    [ExtractFileName(fProfile.FileName)]),mtWarning,mbYesNoCancel,0) of
      mrYes: cmdSaveProfile.Execute;
      mrCancel: result:=false;
    end;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CreckProfileSaved;
end;

procedure TFrmMain.cmdAutoRefreshExecute(Sender: TObject);
begin
  Application.ProcessMessages; // do nothing
end;

procedure TFrmMain.cmdFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.UpdateProfile;
begin
  if Assigned(FrmProfile) then
    FrmProfile.UpdateProfile;
end;

procedure TFrmMain.LoadProfile(const aFileName: string);
begin
  fProfile.ReadFromFile(aFileName);
  UpdateProfile;
end;

procedure TFrmMain.ProfileChange(Sender: TObject);
begin
  cmdSaveProfile.Update;
  UpdateTitle;
  if cmdAutoRefresh.Checked then
    cmdRun.Execute;
end;

procedure TFrmMain.UpdateTitle;
const
  MD: array[boolean] of string = ('','*');
begin
  Caption:=Format('%sGrape - [%s]',[MD[fProfile.Changed],fProfile.FileName]);
  apMain.Title:=Caption;
end;

procedure TFrmMain.cmdFileOpenProfileExecute(Sender: TObject);
begin
  if not CreckProfileSaved then
    exit;
  if DirectoryExists(fProfilesDir) then
    dlgOpenProfile.InitialDir:=fProfilesDir;
  if not dlgOpenProfile.Execute then
    exit;
  Application.ProcessMessages;
  fProfilesDir:=ExtractFilePath(dlgOpenProfile.FileName);
  LoadProfile(dlgOpenProfile.FileName);
end;

procedure TFrmMain.cmdSaveProfileAsExecute(Sender: TObject);
begin
  if not dlgSaveProfile.Execute then
    exit;
  fProfile.SaveToFile(dlgSaveProfile.FileName);
end;

procedure TFrmMain.cmdSaveProfileExecute(Sender: TObject);
begin
  if not FileExists(fProfile.FileName) then begin
    cmdSaveProfileAs.Execute;
    exit;
  end;
  with fProfile do
    SaveToFile(FileName);
end;

procedure TFrmMain.cmdSaveProfileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=fProfile.Changed;
end;

procedure TFrmMain.cmdFileOpenImageExecute(Sender: TObject);
begin
  if DirectoryExists(fImagesDir) then
    dlgOpenImage.InitialDir:=fImagesDir;
  if not dlgOpenImage.Execute then
     exit;
  Application.ProcessMessages;
  fImagesDir:=ExtractFilePath(dlgOpenImage.FileName);
  fImgFileName:=dlgOpenImage.FileName;
  Screen.Cursor:=crHourGlass;
  try

    ImgInput.Picture.LoadFromFile(dlgOpenImage.FileName);
    sbInputResize(sbInput);
    with ImgInput.Picture.Bitmap do begin
      sbMain.Panels[0].Text:=Format('%s (%dx%d)',
        [ExtractFileName(fImgFileName),Width,Height]);
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;


procedure TFrmMain.ClearHoles;
var
  i: integer;
begin
  for i:=0 to fHoles.Count-1 do
    FreeMem(fHoles[i]);
  fHoles.Clear;
end;

procedure TFrmMain.MakeColorMask;
var
  x,y: Integer;
begin
  for y:=0 to fImage.Height-1 do
	  for x:=0 to fImage.Width-1 do
      fMask[x,y]:=(fImage.TColors[x,y]=fProfile.MaskColor);
end;

function TFrmMain.GetSquareIntensity(const a_x, a_y: Integer): Double;
var
  x,y,w: Integer;
  total: Double;
begin
  w:=ceil(fProfile.MaxSize);
  total:=0;
		for y:=0 to w-1 do
			for x:=0 to w-1 do begin
        if (a_x+x>0) and (a_x+x<fImage.Width)
          and (a_y+y>0) and (a_y+y<fImage.Height) then
            total+=255-Red(fImage.TColors[a_x+x,a_y+y]);
      end;
  with fProfile do
	  result:=(total/(MaxSize*MaxSize*255));
end;

procedure TFrmMain.ConvertToGrayScale;
var
  c: TFPColor;
  Gray,x,y: Integer;
begin
  for y:=0 to fImage.Height-1 do
  begin
    for x:=0 to fImage.Width-1 do
    begin
      c:=fImage.Colors[x,y];
      Gray:=Round(c.red*0.3+c.green*0.59+c.blue*0.11);
      if (Gray>0) and (fProfile.Gamma>0) then
        Gray:=Round(Exp(Ln(Gray/65535)/fProfile.Gamma)*65535);
      if fProfile.Inverted then
        Gray:=65535-Gray;
      c.red:=Gray;
      c.green:=Gray;
      c.blue:=Gray;
      fImage.Colors[x,y]:=c;
    end;
  end;
end;

procedure TFrmMain.Process;
var
  x,y,s,f,k,intensity,
  diametr: Double;
  ix,iy,n: Integer;
  hole: PHole;
  Rhombus: array[0..7] of Double;
  fd: TRawImageDescription;
begin
  ClearHoles;

  with ImgInput.Picture.Bitmap do begin
    fd.Init_BPP32_B8G8R8_BIO_TTB(Width,Height);
    fImage.DataDescription:=fd;
    fImage.LoadFromBitmap(Handle,MaskHandle);
  end;

  if fProfile.UseColorMask then begin
    SetLength(fMask,fImage.Width,fImage.Height);
    MakeColorMask;
  end;

  if fProfile.UseHoleSet then
    fProfile.MakeLUT;

  ConvertToGrayScale;

  s:=fProfile.MaxSize+fProfile.Distance;
  f:=s*0.5;
  ix:=Ceil(s*Floor(fImage.Width/s));
  iy:=Ceil(s*Floor(fImage.Height/s));
  if Assigned(fAgg) then
    fAgg.Free;
  fAgg:=TAggLCLCanvas.Create;
  fAgg.Image.PixelFormat:=afpimRGBA32;
  fAgg.Image.SetSize(ix,iy);
  fAgg.AggClearAll(255,255,255);
  fAgg.Brush.Color:=clBlack;
  fAgg.Pen.Width:=0;

  y:=0.0;
  while (y<fImage.Height) do begin
    x:=0.0;
    while (x<fImage.Width) do begin
      ix:=Round(x);
      iy:=Round(y);

      if fProfile.UseColorMask and fMask[ix,iy] then begin
        x+=s;
        continue;
      end;

      intensity:=GetSquareIntensity(ix,iy);
      intensity+=1.0/fProfile.MaxSize;
      diametr:=intensity*fProfile.MaxSize;

      if (diametr<fProfile.MinSize) and fProfile.IgnoreMin then begin
        x+=s;
        continue;
      end;

      diametr:=Max(fProfile.MinSize,diametr);
      hole:=GetMem(SizeOf(THole));
      hole^.x:=x+f;
      hole^.y:=y+f;
      hole^.radius:=diametr*0.5;
      if fProfile.UseHoleSet then begin
        n:=fProfile.FindSizeIndex(diametr);
        if n<0 then begin
          x+=s;
          continue;
        end;
        hole^.radius:=fProfile.Sizes[n].ScreenSize*0.5;
        fAgg.Brush.Color:=fProfile.HoleColor(n);
      end;


      with hole^ do
        case fProfile.Shape of
          hsCircle: fAgg.AggEllipse(x,y,radius,radius);
          hsSquare: begin
            k:=radius;
            //k:=sqrt(pi*radius*radius)*0.5;
            fAgg.AggRectangle(x-k,y-k,x+k,y+k);
          end;
          hsRhombus: begin
            k:=radius;
            //sqrt(pi*radius*radius)*COS45;
            Rhombus[0]:=x-k;
            Rhombus[1]:=y;
            Rhombus[2]:=x;
            Rhombus[3]:=y-k;
            Rhombus[4]:=x+k;
            Rhombus[5]:=y;
            Rhombus[6]:=x;
            Rhombus[7]:=y+k;
            fAgg.AggPolygon(@Rhombus[0],4);
          end;
        end;

      fHoles.Add(hole);
      x+=s;
    end;
    y+=s;
  end;
  {$ifdef Linux}
  with fAgg.Image do
    IntfImg.DataDescription.Init_BPP32_R8G8B8A8_BIO_TTB(Width,Height);
  {$endif}
  // convert to LCL native pixel format
  ImgOutput.Picture.Bitmap.LoadFromIntfImage(fAgg.Image.IntfImg);
  sbInputResize(sbOutput);
end;

procedure TFrmMain.cmdRunExecute(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    Process;
  finally
    Screen.Cursor:=crDefault;
  end;
  sbMain.Panels[1].Text:=Format('%d holes',[fHoles.Count]);
  sbMain.Panels[2].Text:='Conversion done';
end;

procedure TFrmMain.cmdRunUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=not ImgInput.Picture.Bitmap.Empty;
end;

procedure TFrmMain.apMainHint(Sender: TObject);
begin
  sbMain.Panels[2].Text:=Application.Hint;
end;

procedure TFrmMain.cmdAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TFrmMain.cmdEditProfileExecute(Sender: TObject);
begin
  FrmProfile.ShowOnTop;
end;

procedure TFrmMain.cmdHomepageExecute(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru');
end;

procedure TFrmMain.cmdOutputImageCopyExecute(Sender: TObject);
begin
  Clipboard.Assign(ImgOutput.Picture.Bitmap);
end;

procedure TFrmMain.cmdOutputImageCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=not ImgOutput.Picture.Bitmap.Empty;
end;

procedure TFrmMain.cmdExportVectorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=fHoles.Count>0;
end;

procedure TFrmMain.cmdExportVectorExecute(Sender: TObject);
var
  dxf: TDxfWriter;
  i: integer;
  hole: PHole;
begin
  if DirectoryExists(fVectorsDir) then
    dlgExportVector.InitialDir:=fVectorsDir;
  dlgExportVector.FileName:=ChangeFileExt(ExtractFileName(fImgFileName),'.dxf');
  if not dlgExportVector.Execute then
    exit;
  Application.ProcessMessages;
  fVectorsDir:=ExtractFilePath(dlgExportVector.FileName);
  Screen.Cursor:=crHourGlass;
  try
    dxf:=TDxfWriter.Create;
    try
      dxf.DxfBegin;
      for i:=0 to fHoles.Count-1 do begin
        hole:=PHole(fHoles[i]);
        with hole^ do
          case fProfile.Shape of
            hsCircle:  dxf.Circle(x,y,radius);
            hsSquare:  dxf.Square(x,y,radius);   //sqrt(pi*radius*radius)*0.5
            hsRhombus: dxf.Rhombus(x,y,radius);
          end;
      end;
      dxf.DxfEnd;
      dxf.SaveToFile(dlgExportVector.FileName);
    finally
      dxf.Free;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
  sbMain.Panels[2].Text:='Write DXF-file done';
end;

end.

