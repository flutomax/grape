{ This program is free software. You are allowed to redistribute this
  software and making the software available for download or
  making this software part of a software CD compilation.
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any
  damages arising from the use of this software.

  © Copyright 2018 Vasily Makarov
}

unit uGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const

  COS45 = 0.7071067811865475244; // cos 45°

  HOLES_CLRS: array[0..15] of TColor = (
  $000000, $000080, $008000, $008080,
  $800000, $800080, $808000, $808080,
  $C0C0C0, $0000FF, $00FF00, $00FFFF,
  $0080FF, $FF0000, $FF00FF, $FFFF00);

  HOLES_CNAMES: array[0..15] of string = (
  'Black','Maroon','Green','Olive',
  'Navy','Purple','Teal','Gray',
  'Silver','Red','Lime','Yellow',
  'Orange','Blue','Fuchsia','Aqua');

type

  THoleShape = (hsCircle, hsSquare, hsRhombus);

  PHole = ^THole;
  THole = packed record
    x: Double;
    y: Double;
    radius: Double;
  end;

const
  HOLE_SHAPES: array[THoleShape] of string = ('Circle','Square','Rhombus');

implementation


end.

