object FormTest: TFormTest
  Left = 0
  Top = 0
  Caption = 'ProjectProperties - Test'
  ClientHeight = 167
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 64
    Width = 129
    Height = 25
    Caption = 'Test Inno Setup'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 272
    Top = 64
    Width = 129
    Height = 25
    Caption = 'Test NSIS'
    TabOrder = 1
    OnClick = Button2Click
  end
end
