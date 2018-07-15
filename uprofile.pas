{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uProfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles, Contnrs, uGlobal;

type

  TProfile = class;

  { TSizeItem }

  TSizeItem = class
  private
    fEnabled: Boolean;
    fScreenSize: Double;
    fRealSize: Double;
    fColorIndex: Byte;
    fMinimum: Double;
    fOwner: TProfile;
    procedure SetColorIndex(AValue: Byte);
    procedure SetEnabled(AValue: Boolean);
    procedure SetRealSize(AValue: Double);
    procedure SetScreenSize(AValue: Double);
  public
    constructor Create(aOwner: TProfile);
    property Enabled: Boolean read fEnabled write SetEnabled;
    property ScreenSize: Double read fScreenSize write SetScreenSize;
    property RealSize: Double read fRealSize write SetRealSize;
    property ColorIndex: Byte read fColorIndex write SetColorIndex;
    property Minimum: Double read fMinimum write fMinimum;
  end;

  { TIniFileEx }

  TIniFileEx = class(TMemIniFile)
  private
    fFs: TFormatSettings;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds: Boolean = False); overload; override;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; override;
    procedure WriteFloat(const Section, Ident: string; Value: Double); override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
    procedure WriteColor(const Section, Ident: string; Value: TColor);
    function ReadShape(const Section, Ident: string; Default: THoleShape): THoleShape;
    procedure WriteShape(const Section, Ident: string; Value: THoleShape);
    procedure ReadSizeItem(const Section, Ident: string; aItem: TSizeItem);
    procedure WriteSizeItem(const Section, Ident: string; aItem: TSizeItem);
  end;

  { TProfile }

  TProfile = class
  private
    fShape: THoleShape;
    fMaxIndex: Integer;
    fMaxSize: Double;
    fMinSize: Double;
    fDistance: Double;
    fIgnoreMin: Boolean;
    fGamma: Double;
    fInverted: Boolean;
    fUseColorMask: Boolean;
    fMaskColor: TColor;
    fUseHoleSet: Boolean;
    fSizeList: TObjectList;
    fFileName: string;
    fUpdateCount: Integer;
    fChanged: Boolean;
    fOnChange: TNotifyEvent;
    function AddSizeInternal: TSizeItem;
    function GetSizes(const Index: Integer): TSizeItem;
    function GetSizesCount: Integer;
    procedure DoChange;
    procedure SetDistance(AValue: Double);
    procedure SetGamma(AValue: Double);
    procedure SetIgnoreMin(AValue: Boolean);
    procedure SetInverted(AValue: Boolean);
    procedure SetMaskColor(AValue: TColor);
    procedure SetMaxSize(AValue: Double);
    procedure SetMinSize(AValue: Double);
    procedure SetShape(AValue: THoleShape);
    procedure SetUseColorMask(AValue: Boolean);
    procedure SetUseHoleSet(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DeleteSize(const Index: Integer);
    procedure RemoveSize(aItem: TSizeItem);
    procedure MakeLUT;
    function AddSize: TSizeItem;
    function FindSizeIndex(const average: Double): Integer;
    function HoleColor(const Index: Integer): TColor;
    property Sizes[const Index: Integer]: TSizeItem read GetSizes;
  published
    property Changed: Boolean read fChanged;
    property Shape: THoleShape read fShape write SetShape;
    property MaxSize: Double read fMaxSize write SetMaxSize;
    property MinSize: Double read fMinSize write SetMinSize;
    property Distance: Double read fDistance write SetDistance;
    property IgnoreMin: Boolean read fIgnoreMin write SetIgnoreMin;
    property Gamma: Double read fGamma write SetGamma;
    property Inverted: Boolean read fInverted write SetInverted;
    property UseColorMask: Boolean read fUseColorMask write SetUseColorMask;
    property MaskColor: TColor read fMaskColor write SetMaskColor;
    property UseHoleSet: Boolean read fUseHoleSet write SetUseHoleSet;
    property SizesCount: Integer read GetSizesCount;
    property FileName: string read fFileName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

uses
  Math;

{ TSizeItem }

constructor TSizeItem.Create(aOwner: TProfile);
begin
  fOwner:=aOwner;
  fEnabled:=true;
  fRealSize:=20;
  fScreenSize:=2;
  fColorIndex:=0;
  fMinimum:=0;
end;

procedure TSizeItem.SetColorIndex(AValue: Byte);
begin
  if fColorIndex=AValue then Exit;
  fColorIndex:=AValue;
  fOwner.DoChange;
end;

procedure TSizeItem.SetEnabled(AValue: Boolean);
begin
  if fEnabled=AValue then Exit;
  fEnabled:=AValue;
  fOwner.DoChange;
end;

procedure TSizeItem.SetRealSize(AValue: Double);
begin
  if fRealSize=AValue then Exit;
  fRealSize:=AValue;
  fOwner.DoChange;
end;

procedure TSizeItem.SetScreenSize(AValue: Double);
begin
  if fScreenSize=AValue then Exit;
  fScreenSize:=AValue;
  fOwner.DoChange;
end;


{ TIniFileEx }

constructor TIniFileEx.Create(const AFileName: string; AEscapeLineFeeds: Boolean);
begin
  inherited Create(AFileName, AEscapeLineFeeds);
  fFs:=DefaultFormatSettings;
  fFs.DecimalSeparator:='.';
  fFs.ThousandSeparator:='#';// disable the thousand separator
end;

function TIniFileEx.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result:=StrToFloatDef(ReadString(Section,Ident,''),Default,fFs);
end;

procedure TIniFileEx.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section,Ident,FloatToStrF(Value,ffgeneral,18,22,fFs));
end;

function TIniFileEx.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result:=StrToBoolDef(ReadString(Section,Ident,''),Default);
end;

procedure TIniFileEx.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section,Ident,BoolToStr(Value,true));
end;

function TIniFileEx.ReadColor(const Section, Ident: string; Default: TColor): TColor;
begin
  Result:=StringToColorDef(ReadString(Section,Ident,''),Default);
end;

procedure TIniFileEx.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section,Ident,ColorToString(Value));
end;

function TIniFileEx.ReadShape(const Section, Ident: string; Default: THoleShape): THoleShape;
var
  s: string;
begin
  s:=ReadString(Section,Ident,'');
  for result:=Low(THoleShape) to High(THoleShape) do
    if s=HOLE_SHAPES[result] then
      exit;
  result:=Default;
end;

procedure TIniFileEx.WriteShape(const Section, Ident: string; Value: THoleShape);
begin
  WriteString(Section,Ident,HOLE_SHAPES[Value]);
end;

procedure TIniFileEx.ReadSizeItem(const Section, Ident: string; aItem: TSizeItem);
var
  s: TStringList;
begin
  s:=TStringList.Create;
  try
    s.CommaText:=ReadString(Section,Ident,'');
    if s.Count=4 then begin
      aItem.Enabled:=StrToBoolDef(s[0],false);
      aItem.ScreenSize:=StrToFloatDef(s[1],0,fFs);
      aItem.RealSize:=StrToFloatDef(s[2],0,fFs);
      aItem.ColorIndex:=StrToIntDef(s[3],0);
    end;
  finally
    s.Free;
  end;
end;

procedure TIniFileEx.WriteSizeItem(const Section, Ident: string; aItem: TSizeItem);
begin
  with aItem do
    WriteString(Section,Ident,Format('%s, %g, %g, %d',
      [BoolToStr(Enabled,true),ScreenSize,RealSize,ColorIndex],fFs));
end;

const
  DEF_FILENAME = 'untitled.gpf';
  DEF_SHAPE = hsCircle;
  DEF_MAXSIZE = 6.0;
  DEF_MINSIZE = 1.0;
  DEF_DISTANCE = 1.0;
  DEF_IGNOREMIN = false;
  DEF_GAMMA = 1.0;
  DEF_INVERTED = false;
  DEF_USEHOLESET = false;
  DEF_USECOLORMASK = false;
  DEF_MASKCOLOR = clFuchsia;

{ TProfile }

constructor TProfile.Create;
var
  aItem: TSizeItem;
  i: integer;
begin
  fSizeList:=TObjectList.Create(true);
  fUpdateCount:=0;
  fChanged:=false;
  fFileName:=DEF_FILENAME;
  fShape:=DEF_SHAPE;
  fMaxSize:=DEF_MAXSIZE;
  fMinSize:=DEF_MINSIZE;
  fDistance:=DEF_DISTANCE;
  fIgnoreMin:=DEF_IGNOREMIN;
  fGamma:=DEF_GAMMA;
  fInverted:=DEF_INVERTED;
  fUseHoleSet:=DEF_USEHOLESET;
  fUseColorMask:=DEF_USECOLORMASK;
  fMaskColor:=DEF_MASKCOLOR;
  BeginUpdate;
  try
    for i:=0 to 4 do begin
      aItem:=AddSizeInternal;
      aItem.ScreenSize:=i+2;
      aItem.RealSize:=aItem.ScreenSize*10;
      aItem.ColorIndex:=i+9;
    end;
  finally
    EndUpdate;
  end;
end;

destructor TProfile.Destroy;
begin
  fSizeList.Free;
  inherited Destroy;
end;

procedure TProfile.ReadFromFile(const aFileName: string);
var
  ini: TIniFileEx;
  i,n: Integer;
  aItem: TSizeItem;
begin
  BeginUpdate;
  try
    fSizeList.Clear;
    ini:=TIniFileEx.Create(aFileName);
    try
      fShape:=ini.ReadShape('Holes','Shape',DEF_SHAPE);
      fMaxSize:=ini.ReadFloat('Holes','MaxSize',DEF_MAXSIZE);
      fMinSize:=ini.ReadFloat('Holes','MinSize',DEF_MINSIZE);
      fDistance:=ini.ReadFloat('Holes','Distance',DEF_DISTANCE);
      fIgnoreMin:=ini.ReadBool('Holes','IgnoreMin',DEF_IGNOREMIN);
      fGamma:=ini.ReadFloat('Image','Gamma',DEF_GAMMA);
      fInverted:=ini.ReadBool('Image','Inverted',DEF_INVERTED);
      fUseColorMask:=ini.ReadBool('Image','UseColorMask',DEF_USECOLORMASK);
      fMaskColor:=ini.ReadColor('Image','MaskColor',DEF_MASKCOLOR);
      fUseHoleSet:=ini.ReadBool('HoleSet','Use',DEF_USEHOLESET);

      n:=ini.ReadInteger('HoleSet','Count',0);
      for i:=0 to n-1 do begin
        aItem:=AddSizeInternal;
        ini.ReadSizeItem('HoleSet',Format('Size%d',[i+1]),aItem);;
      end;
    finally
      ini.Free;
    end;
  finally
    EndUpdate;
  end;
  fFileName:=aFileName;
  fChanged:=false;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TProfile.SaveToFile(const aFileName: string);
var
  ini: TIniFileEx;
  i: Integer;
begin
  ini:=TIniFileEx.Create(aFileName);
  try
    ini.WriteShape('Holes','Shape',fShape);
    ini.WriteFloat('Holes','MaxSize',fMaxSize);
    ini.WriteFloat('Holes','MinSize',fMinSize);
    ini.WriteFloat('Holes','Distance',fDistance);
    ini.WriteBool('Holes','IgnoreMin',fIgnoreMin);
    ini.WriteFloat('Image','Gamma',fGamma);
    ini.WriteBool('Image','Inverted',fInverted);
    ini.WriteBool('Image','UseColorMask',fUseColorMask);
    ini.WriteColor('Image','MaskColor',fMaskColor);

    ini.WriteBool('HoleSet','Use',fUseHoleSet);
    ini.WriteInteger('HoleSet','Count',fSizeList.Count);

    for i:=0 to fSizeList.Count-1 do
      ini.WriteSizeItem('HoleSet',Format('Size%d',[i+1]),GetSizes(i));

    ini.UpdateFile;
  finally
    ini.Free;
  end;
  fFileName:=aFileName;
  fChanged:=false;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function SortSizeList(Item1, Item2: Pointer): Integer;
begin
  if TSizeItem(Item1).ScreenSize>TSizeItem(Item2).ScreenSize then
    result:=1
  else
    if TSizeItem(Item1).ScreenSize<TSizeItem(Item2).ScreenSize then
      result:=-1
    else
      result:=0;
end;

function TProfile.AddSizeInternal: TSizeItem;
begin
  result:=TSizeItem.Create(self);
  fSizeList.Add(result);
end;

function TProfile.AddSize: TSizeItem;
begin
  result:=AddSizeInternal;
  fSizeList.Sort(@SortSizeList);
  DoChange;
end;

procedure TProfile.DeleteSize(const Index: Integer);
begin
  fSizeList.Delete(Index);
  DoChange;
end;

procedure TProfile.RemoveSize(aItem: TSizeItem);
begin
  fSizeList.Remove(aItem);
end;

procedure TProfile.MakeLUT;
var
  i,k,n: Integer;
  aMin,aMax: Double;
begin
  aMax:=fMaxSize;
  fMaxIndex:=-1;
  for i:=fSizeList.Count-1 downto 1 do begin
    if not Sizes[i].Enabled then
      continue;
    if fMaxIndex<0 then
      fMaxIndex:=i;
    k:=i;
    repeat
      Dec(k);
      aMin:=Sizes[k].ScreenSize;
    until (Sizes[k].Enabled) or (k=0);
    Sizes[i].Minimum:=(aMax+aMin)/2;
    aMax:=aMin;
  end;
end;

function TProfile.FindSizeIndex(const average: Double): Integer;
var
  i,n: Integer;
begin
  result:=-1;
  if (average<fMinSize) and fIgnoreMin then
    exit;
  if (not UseHoleSet) then
    Exit;
  result:=fMaxIndex;
  for i:=fSizeList.Count-1 downto 0 do begin
    if not Sizes[i].Enabled then
      continue;
    if average>=Sizes[i].Minimum then
      exit;
    result:=i;
  end;
end;

function TProfile.HoleColor(const Index: Integer): TColor;
begin
  result:=HOLES_CLRS[GetSizes(Index).ColorIndex];
end;

procedure TProfile.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TProfile.EndUpdate;
begin
  Dec(fUpdateCount);
end;

procedure TProfile.DoChange;
begin
  if fUpdateCount>0 then
    exit;
  fChanged:=true;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TProfile.GetSizes(const Index: Integer): TSizeItem;
begin
  result:=TSizeItem(fSizeList[Index]);
end;

function TProfile.GetSizesCount: Integer;
begin
  result:=fSizeList.Count;
end;

procedure TProfile.SetUseColorMask(AValue: Boolean);
begin
  if fUseColorMask=AValue then Exit;
  fUseColorMask:=AValue;
  DoChange;
end;

procedure TProfile.SetUseHoleSet(AValue: Boolean);
begin
  if fUseHoleSet=AValue then Exit;
  fUseHoleSet:=AValue;
  DoChange;
end;

procedure TProfile.SetDistance(AValue: Double);
begin
  if fDistance=AValue then Exit;
  fDistance:=AValue;
  DoChange;
end;

procedure TProfile.SetGamma(AValue: Double);
begin
  if fGamma=AValue then Exit;
  fGamma:=EnsureRange(AValue,0.1,3.0);
  DoChange;
end;

procedure TProfile.SetIgnoreMin(AValue: Boolean);
begin
  if fIgnoreMin=AValue then Exit;
  fIgnoreMin:=AValue;
  DoChange;
end;

procedure TProfile.SetInverted(AValue: Boolean);
begin
  if fInverted=AValue then Exit;
  fInverted:=AValue;
  DoChange;
end;

procedure TProfile.SetMaskColor(AValue: TColor);
begin
  if fMaskColor=AValue then Exit;
  fMaskColor:=AValue;
  DoChange;
end;

procedure TProfile.SetMaxSize(AValue: Double);
begin
  if fMaxSize=AValue then Exit;
  fMaxSize:=AValue;
  DoChange;
end;

procedure TProfile.SetMinSize(AValue: Double);
begin
  if fMinSize=AValue then Exit;
  fMinSize:=AValue;
  DoChange;
end;

procedure TProfile.SetShape(AValue: THoleShape);
begin
  if fShape=AValue then Exit;
  fShape:=AValue;
  DoChange;
end;

end.

