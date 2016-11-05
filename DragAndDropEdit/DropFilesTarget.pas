{
  DropFilesTarget.pas

  Delphi unit containing the non-visual component TDropFilesTarget which
  implements the DragAcceptFiles / WM_DROPFILES API for any control that
  has a window handle (TWinControl descendant).

  Version 1.2a - always find the most current version at
  http://flocke.vssd.de/prog/code/pascal/dft/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
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

unit DropFilesTarget;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ShellApi, Graphics, StdCtrls;

const
  colorDragObject           = clInfoBk;
  colorNoDragObject         = clWindow;
  colorDragObjectDropOK     = clMoneyGreen;
  colorDragObjectDropFail   = $5555e5;
  ECM_FIRST                 = $1500;
  EM_SETCUEBANNER           = ECM_FIRST + 1;

type
  TDropFilesInfo = class(TPersistent)
  private
    FControl: TWinControl;          // Drop action target control
    FStamp: TDateTime;              // Timestamp of drop action
    FPoint: TPoint;                 // Drop point
    FFiles: TStrings;               // List with filenames
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Control: TWinControl read FControl;
    property Files: TStrings read FFiles;
    property Point: TPoint read FPoint;
    property Stamp: TDateTime read FStamp;
  end;

  TDropFilesEvent = procedure(Sender: TObject; Info: TDropFilesInfo) of object;

  TDropFilesTarget = class(TComponent)
  private
    FTargetControl: TWinControl;    // Target control to accept WM_DROPFILES
    FEnabled: Boolean;              // Enable/disable accepting
    FOnDropFiles: TDropFilesEvent;  // Notification handler
    FAcceptingWindow: HWND;         // Window handle that got "DragAcceptFiles"
    FOldWndProc: TWndMethod;        // Old WindowProc method
    procedure DropFiles(hDrop: HDROP);
    procedure NewWndProc(var Msg: TMessage);
    procedure AttachControl;
    procedure DetachControl;
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetTargetControl(AControl: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCueText(Text: String);
    procedure DragDropSuccess();
    procedure DragDropFailed();
    procedure DragDropRegular();

  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property TargetControl: TWinControl read FTargetControl write SetTargetControl;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
  end;

procedure Register;

implementation

{ TDropFilesInfo }

constructor TDropFilesInfo.Create;
begin
  inherited;
  FFiles := TStringList.Create;
end;

destructor TDropFilesInfo.Destroy;
begin
  FreeAndNil(FFiles);
  inherited;
end;

procedure TDropFilesInfo.Assign(Source: TPersistent);
begin
  if Source is TDropFilesInfo then
  begin
    FControl := TDropFilesInfo(Source).Control;
    FStamp := TDropFilesInfo(Source).Stamp;
    FPoint := TDropFilesInfo(Source).Point;
    FFiles.Assign(TDropFilesInfo(Source).Files);
  end
  else
    inherited Assign(Source);
end;

{ TDropFilesTarget }

constructor TDropFilesTarget.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
end;

destructor TDropFilesTarget.Destroy;
begin
  TargetControl := nil;  // This detaches any attached control
  inherited;
end;

procedure TDropFilesTarget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FTargetControl then
      TargetControl := nil;
end;

{ Do the dropping. Note that DragFinish is called in the window
  procedure and not here.
}
procedure TDropFilesTarget.DropFiles(hDrop: HDROP);
var
  Info: TDropFilesInfo;
  Count, Index, Len: Integer;
  Filename: PChar;
begin
  Info := TDropFilesInfo.Create;
  try
    Info.FStamp := Now;
    Info.FControl := FTargetControl;
    DragQueryPoint(hDrop, Info.FPoint);

    Count := DragQueryFile(hDrop, $ffffffff, nil, 0);
    for Index := 0 to Count - 1 do
    begin
      Len := DragQueryFile(hDrop, Index, nil, 0);
      Filename := AllocMem(Len + 1);
      try
        DragQueryFile(hDrop, Index, Filename, Len + 1);
        TStringList(Info.FFiles).Add(StrPas(Filename));
      finally
        FreeMem(Filename);
      end;
    end;

    FOnDropFiles(Self, Info);
  finally
    Info.Free;
  end;
end;

{ The new Window procedure for the attached drop target control.
}
procedure TDropFilesTarget.NewWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DROPFILES then
  begin
    try
      if Assigned(FOnDropFiles) then
        DropFiles(Msg.WParam);
    finally
      DragFinish(Msg.WParam);
    end;
    Msg.Result := 0;
  end
  else
  begin
    if Msg.Msg = WM_DESTROY then
      if FAcceptingWindow <> 0 then
        // Don't clear FAcceptingWindow
        DragAcceptFiles(FAcceptingWindow, false);

    FOldWndProc(Msg);

    if Msg.Msg = WM_CREATE then
      // Make it believe the window handle must be refreshed
      FAcceptingWindow := 0;

    if FTargetControl.HandleAllocated then
      if FAcceptingWindow <> FTargetControl.Handle then
      begin
        FAcceptingWindow := FTargetControl.Handle;
        DragAcceptFiles(FAcceptingWindow, true);
      end;
  end;
end;

{ Attach to FTargetControl: subclass, force a window handle and call
  DragAcceptFiles with Accept=true.
}
procedure TDropFilesTarget.AttachControl;
var
    TempControl: TEdit;
begin
  if [csDesigning, csDestroying] * ComponentState <> [] then
    exit;

  if (FTargetControl = nil) {or (not FEnabled)} then
    exit;

  // Set Drag color
  if(FTargetControl is TEdit) then
  begin
    TempControl := FTargetControl as TEdit;
    TempControl.Color := colorDragObject;
    TempControl.Update;
  end;

  if [csDesigning, csDestroying] * FTargetControl.ComponentState <> [] then
    exit;

  FOldWndProc := FTargetControl.WindowProc;
  FTargetControl.WindowProc := NewWndProc;

  // Note: If we don't force a handle here we get problems with controls
  // that call ReCreateWnd before they even got a handle (-> RichEdit).
  FTargetControl.HandleNeeded;
  FAcceptingWindow := FTargetControl.Handle;
  DragAcceptFiles(FAcceptingWindow, true);
end;

{ Detach from FTargetControl: call DragAcceptFiles with Accept=false and
  remove subclassing.
}
procedure TDropFilesTarget.DetachControl;
var
    TempControl: TEdit;
begin
  if FAcceptingWindow <> 0 then
  begin
    DragAcceptFiles(FAcceptingWindow, false);
    FAcceptingWindow := 0;
  end;

  if @FOldWndProc <> nil then
  begin
    FTargetControl.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;

  // Remove Drag color
  if(FTargetControl is TEdit) then
  begin
    TempControl := FTargetControl as TEdit;
    //TempControl.Color := colorNoDragObject;
    TempControl.Update;
  end;
end;

procedure TDropFilesTarget.SetEnabled(AEnabled: Boolean);
begin
  if FEnabled <> AEnabled then
  begin
    DetachControl;
    FEnabled := AEnabled;
    AttachControl;
  end;
end;

procedure TDropFilesTarget.SetTargetControl(AControl: TWinControl);
begin
  if FTargetControl <> AControl then
  begin
    DetachControl;

    if FTargetControl <> nil then
      FTargetControl.RemoveFreeNotification(Self);

    FTargetControl := AControl;

    if FTargetControl <> nil then
      FTargetControl.FreeNotification(Self);

    AttachControl;
  end;
end;

{ Register }

procedure Register;
begin
  RegisterComponents('System', [TDropFilesTarget]);
end;

procedure TDropFilesTarget.DragDropSuccess();
var
    TempControl: TEdit;
begin
  // Set Drag color (Operation succeeded)
  if(FTargetControl is TEdit) then
  begin
    TempControl := FTargetControl as TEdit;
    TempControl.Color := colorDragObjectDropOK;
    TempControl.Update;
  end;
end;

procedure TDropFilesTarget.DragDropFailed();
var
    TempControl: TEdit;
begin
  // Set Drag color (Operation failed)
  if(FTargetControl is TEdit) then
  begin
    TempControl := FTargetControl as TEdit;
    TempControl.Color := colorDragObjectDropFail;
    TempControl.Update;
  end;
end;

procedure TDropFilesTarget.DragDropRegular();
var
    TempControl: TEdit;
begin
  // Set Drag color (Normal color)
  if(FTargetControl is TEdit) then
  begin
    TempControl := FTargetControl as TEdit;
    TempControl.Color := colorDragObject;
    TempControl.Update;
  end;
end;

procedure TDropFilesTarget.SetCueText(Text: String);
begin
    // Sets Cue Text for TEdit controls (http://msdn.microsoft.com/en-us/library/windows/desktop/bb761639%28v=vs.85%29.aspx)
    // Does not work correctly on Win XP
    // http://stackoverflow.com/questions/1465845/cuetext-equivalent-for-a-tmemo
    if ((FTargetControl <> nil) and (FTargetControl is TEdit)) then
      SendMessage(FTargetControl.Handle, EM_SETCUEBANNER, 0, LParam(PWideChar(WideString(Text))));
end;

end.
