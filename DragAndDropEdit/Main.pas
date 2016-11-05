{
  Drag & Drop (TEdit) sample

  This example shows how to use the non-visual component TDropFilesTarget with TEdit and other controls.
  It enhances TDropFilesTarget with Success/Failed Coloring and Cue text feature.
  Drag & Drop also works in Win Vista/7/8/10 from UAC to non-UAC apps (see comments)

  Version 1.0 - always find the most current version at
  http://delphicomponents.codeplex.com/

  This component was developed for Graphical Installer for NSIS and Inno Setup (http://graphical-installer.com/)

  Copyright (C) 2016 unSigned (http://www.unsigned.sk)
  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
}

unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.StrUtils,
  DropFilesTarget;

type
  TFormMain = class(TForm)
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitializeDragAndDropControls();
    procedure DropFilesTargetDrop(Sender: TObject; Info: TDropFilesInfo);
  end;

var
  FormMain: TFormMain;
  DragDropTargets: Array of TDropFilesTarget;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeDragAndDropControls();
end;

procedure TFormMain.InitializeDragAndDropControls();
var
    I: Integer;
    DLLHandle   : THandle;
    FuncFilter  : function (msg: Cardinal; dwFlag: DWORD): BOOL; stdcall;

begin
    // Enable Drag and Drop for Win Vista/7/8/10 from UAC to non-UAC: http://helgeklein.com/blog/2010/03/how-to-enable-drag-and-drop-for-an-elevated-mfc-application-on-vistawindows-7/
    // http://mrxray.on.coocan.jp/Delphi/plSamples/000_CommandLineCompile.htm
    if CheckWin32Version(6, 0) then
    begin
      DLLHandle := loadLibrary('user32.dll');
      try
        if DLLHandle > 0 then
        begin
          @FuncFilter := GetProcAddress(DLLHandle, 'ChangeWindowMessageFilter');
          if @FuncFilter <> nil then
          begin
            FuncFilter(WM_DROPFILES,      MSGFLT_ADD);
            FuncFilter(WM_COPYDATA,       MSGFLT_ADD);
            FuncFilter(WM_COPYGLOBALDATA, MSGFLT_ADD);
          end;
        end;
      finally
        FreeLibrary(DLLHandle);
      end;
    end;

    // All components share the same callback method, but of course callbacks can differ
    SetLength(DragDropTargets, 2);

    for I := 0 To 1 Do
        DragDropTargets[I] := TDropFilesTarget.Create(Self);

    // Register TEdit(s) for dragging files
    DragDropTargets[0].TargetControl := Edit1;
    DragDropTargets[0].SetCueText('Drag & Drop .jpg files here...');

    // Register GroupBox
    DragDropTargets[1].TargetControl := GroupBox1;

    // Attach callback
    for I := 0 To 1 Do
        DragDropTargets[I].OnDropFiles := DropFilesTargetDrop;

    // Set color (for GroupBox it is ignored)
    for I := 0 To 1 Do
        DragDropTargets[I].DragDropRegular();
end;

procedure TFormMain.DropFilesTargetDrop(Sender: TObject; Info: TDropFilesInfo);
var
  DropTarget: TDropFilesTarget;
  FileName: String;

begin
    // Initialize variables (Take only first dragged file)
    DropTarget := Sender as TDropFilesTarget;
    if(Info.Files.Count > 0) then
        FileName := Info.Files.Strings[0]
    else
        Exit;

    if (DropTarget.TargetControl.Handle = Edit1.Handle) then
    begin
        if(AnsiContainsText(FileName, '.jpg'))then
        begin
            Edit1.Text := FileName;
            DropTarget.DragDropSuccess();
        end
        else
        begin
            Edit1.Text := '';
            DropTarget.DragDropFailed();
        end;
    end;

    if (DropTarget.TargetControl.Handle = GroupBox1.Handle) then
    begin
        if(AnsiContainsText(FileName, '.jpg')) then
        begin
            GroupBox1.Caption := FileName;
            // Coloring is ignored for anything than TEdit
            DropTarget.DragDropSuccess();
        end
        else
            DropTarget.DragDropFailed();
    end;
end;

end.
