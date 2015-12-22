object FormTest: TFormTest
  Left = 0
  Top = 0
  Caption = 'NavigationBars - Test'
  ClientHeight = 167
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 112
    Width = 393
    Height = 39
    Caption = 
      'Click Show! and start typing into boxes to show matching items. ' +
      'Click the down arrow to show everything. Press Escape to close t' +
      'he suggestion list.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 160
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Show!'
    TabOrder = 0
    OnClick = Button1Click
  end
end
