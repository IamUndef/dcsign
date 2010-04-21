object MainModule: TMainModule
  Left = 0
  Top = 0
  Caption = 'MainModule'
  ClientHeight = 282
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbMain: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 412
    Height = 276
    Align = alClient
    Caption = #1057#1087#1080#1089#1086#1082' '#1092#1072#1081#1083#1086#1074
    TabOrder = 0
    ExplicitLeft = 136
    ExplicitTop = 64
    ExplicitWidth = 185
    ExplicitHeight = 105
    object sgFiles: TStringGrid
      Left = 2
      Top = 15
      Width = 408
      Height = 259
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      ColCount = 2
      DefaultRowHeight = 18
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect]
      TabOrder = 0
      ExplicitWidth = 414
      ExplicitHeight = 265
      ColWidths = (
        51
        282)
    end
  end
end
