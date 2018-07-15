{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    Button1: TButton;
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    lbVer: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    procedure ExtractMainIcon;
  public
    { public declarations }
  end;

var
  FrmAbout: TFrmAbout;

  procedure ShowAbout;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, IntfGraphics, GraphType, FPimage, fileinfo, resource;


type

  TBGRA = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;
  PBGRA = ^TBGRA;

procedure ShowAbout;
begin
  with TFrmAbout.Create(Application) do
    try
      ShowModal;
    finally
      Release;
    end;
end;

{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
const
  BIT = {$ifdef CPU32}32{$else}64{$endif};
  SYS = {$ifdef Windows}'Windows'{$endif}
        {$ifdef Linux}'Linux'{$endif}
        {$ifdef Darwin}'MacOS'{$endif};
var
  Info: TVersionInfo;
begin
  Info:=TVersionInfo.Create;
  try
    Info.Load(HINSTANCE);
    with Info.FixedInfo do
      lbVer.Caption:=Format('Version %d.%d.%d.%d                %s (%d-bit)',
        [FileVersion[0],FileVersion[1],FileVersion[2],FileVersion[3],SYS,BIT]);
  finally
    Info.Free;
  end;
  ExtractMainIcon;
end;

procedure TFrmAbout.ExtractMainIcon;
var
  strm1: TResourceStream;
  info: PBitmapInfoHeader;
  fd: TRawImageDescription;
  x,y: Integer;
  img: TLazIntfImage;
  p: PBGRA;
  c: TFPColor;
begin
  img:=TLazIntfImage.Create(0,0);
  try
    fd.Init_BPP32_B8G8R8A8_BIO_TTB(128,128);
    img.DataDescription:=fd;
    strm1:=TResourceStream.CreateFromID(hInstance,5,PChar(RT_ICON));
    try
      info:=PBitmapInfoHeader(strm1.Memory);
      p:=Pointer(strm1.Memory+PtrInt(info^.biSize));
      for y:=127 downto 0 do
        for x:=0 to 127 do begin
          c.blue:=p^.Blue shl 8;
          c.green:=p^.Green shl 8;
          c.red:=p^.Red shl 8;
          c.alpha:=p^.Alpha shl 8;
          img.Colors[x,y]:=c;
          inc(p);
        end;
    finally
      strm1.Free;
    end;
  Image.Picture.Bitmap.LoadFromIntfImage(img);
  finally
    img.Free;
  end;
end;


procedure TFrmAbout.Label2Click(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru');
end;

procedure TFrmAbout.Button1Click(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/donation?lang=en');
end;

end.

