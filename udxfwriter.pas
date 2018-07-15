{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uDxfWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDxfWriter }

  TDxfWriter = class(TObject)
  private
    fList: TStringList;
    fFs: TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: TFileName);
    procedure DxfBegin;
    procedure DxfEnd;
    procedure Circle(const x, y, radius: double);
    procedure Line(const x0, y0, x1, y1: double);
    procedure Square(const x, y, size: double);
    procedure Rhombus(const x, y, size: double);
  end;

implementation




{ TDxfWriter }

constructor TDxfWriter.Create;
begin
  fList:=TStringList.Create;
  fFs:=DefaultFormatSettings;
  fFs.DecimalSeparator:='.';
  fFs.ThousandSeparator:='#';// disable the thousand separator
end;

destructor TDxfWriter.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TDxfWriter.SaveToFile(const FileName: TFileName);
begin
  fList.SaveToFile(FileName);
end;

procedure TDxfWriter.DxfBegin;
begin
  fList.Add('0');
  fList.Add('SECTION');
  fList.Add('2');
  fList.Add('ENTITIES');
end;

procedure TDxfWriter.DxfEnd;
begin
  fList.Add('0');
  fList.Add('ENDSEC');
  fList.Add('0');
  fList.Add('EOF');
end;

procedure TDxfWriter.Circle(const x, y, radius: double);
begin
  fList.Add('0');
  fList.Add('CIRCLE');
  fList.Add('8');  // Group code for layer name
  fList.Add('0');  // Layer number (default layer in autocad)
  fList.Add('10'); // XYZ is the Center point of circle
  fList.Add(Format('%g',[x],fFs));  // X in UCS (User Coordinate System) coordinates
  fList.Add('20');
  fList.Add(Format('%g',[y],fFs));  // Y in UCS (User Coordinate System) coordinates
  fList.Add('30');
  fList.Add('0.0');  // Z in UCS (User Coordinate System) coordinates
  fList.Add('40');
  fList.Add(Format('%g',[radius],fFs)); // radius of circle
end;

procedure TDxfWriter.Line(const x0, y0, x1, y1: double);
begin
  fList.Add('0');
  fList.Add('LINE');
  fList.Add('8');  // Group code for layer name
  fList.Add('0');  // Layer number (default layer in autocad)
  fList.Add('10'); // Start point of line
  fList.Add(Format('%g',[x0],fFs));  // X in UCS (User Coordinate System) coordinates
  fList.Add('20');
  fList.Add(Format('%g',[y0],fFs));  // Y in UCS (User Coordinate System) coordinates
  fList.Add('30');
  fList.Add('0.0');  // Z in UCS (User Coordinate System) coordinates
  fList.Add('11');   // End point of line
  fList.Add(Format('%g',[x1],fFs));  // X in UCS (User Coordinate System) coordinates
  fList.Add('21');
  fList.Add(Format('%g',[y1],fFs));  // Y in UCS (User Coordinate System) coordinates
  fList.Add('31');
  fList.Add('0.0');  // Z in UCS (User Coordinate System) coordinates
end;

procedure TDxfWriter.Square(const x, y, size: double);
begin
  Line(x-size,y-size,x+size,y-size);
  Line(x+size,y-size,x+size,y+size);
  Line(x+size,y+size,x-size,y+size);
  Line(x-size,y+size,x-size,y-size);
end;

procedure TDxfWriter.Rhombus(const x, y, size: double);
begin
  Line(x-size,y,x,y-size);
  Line(x,y-size,x+size,y);
  Line(x+size,y,x,y+size);
  Line(x,y+size,x-size,y);
end;

end.

