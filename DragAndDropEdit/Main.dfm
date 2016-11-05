object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Drag & Drop (TEdit) Sample'
  ClientHeight = 132
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 21
    Top = 16
    Width = 516
    Height = 21
    TabOrder = 2
    TextHint = 'gfdgdfgf'
  end
  object GroupBox1: TGroupBox
    Left = 21
    Top = 56
    Width = 516
    Height = 49
    Caption = 
      'You can drag files here too. But coloring is not available (only' +
      ' for TEdit).'
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 664
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit2'
  end
end
