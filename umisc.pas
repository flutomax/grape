{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

procedure MakeDisabledImageList(const Src, Dst: TImageList);


implementation

uses
  LCLType, GraphType;

procedure MakeDisabledImageList(const Src, Dst: TImageList);
var
  i,x,y: integer;
  img: TRawImage;
  p: PRGBAQuad;
begin
  Dst.AddImages(Src);
  for i:=0 to Dst.Count-1 do begin;
    Dst.GetRawImage(i,img);
    p:=PRGBAQuad(img.Data);
    for y:=0 to img.Description.Height-1 do
      for x:=0 to img.Description.Width-1 do begin
        with p^ do begin
           Red:=(Red+Green+Blue) div 3;
           Green:=Red;
           Blue:=Red;
           Alpha:=Alpha div 4;
        end;
        inc(p);
      end;
  end;
end;

end.

