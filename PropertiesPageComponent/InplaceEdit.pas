{
  TInplaceEdit component. Copyright 1996 MIKSoft, Inc.
  **********************************************

  Freeware by MIKSoft, Inc. Uploaded by Mik Kvitchko, mik@cnj.digex.com

  This unit contains one component implementing an in-place editor. The
  component can be used to extend the functionality of the non-editable
  components (like ListBox, for example) by providing the in-place
  editing ability.

  Actually, this component is a "promotionware": you are free to use it
  as you wish in any projects, however we ask you to visit our WWW home
  page and read about our other nice products. Chances are, you'll get
  an extra bonus - right from our home page :-)

                http://www.cnj.digex.net/~mik

                      *********************

  TInplaceEdit does not make an attempt to recognize its parent and to adjust
  its behavior accordingly - it is made as a "base" or "generic"
  in-place editor, you can create a set of specialized in-place editors
  (TListBoxInEdit, TOutlineInEdit etc) by deriving from TInplaceEdit and
  adding the behavior appropriate for the certain parent component.

  The techniques for implementing such customized behavior you will find
  in the accompanying sample - uined1.pas. This unit shows how to make
  an editable listbox.

  TInplaceEdit derives from TEdit and inherits all TEdit's properties.
  It is possible, that some other flavor of edit's control will suit
  your needs better - in such case you simply need to change the source
  code to derive TInplaceEdit from some other edit component.

  Being a generic in-place editor, TInplaceEdit has no way to know where it
  must be placed, and what text it must contain - so it is your responsibility
  to position the component during the run-time and to make it visible at the
  appropriate time - see uined1.pas for example.

  How it works?
  When designing the form, place TInplaceEdit on top of some uneditable component,
  ie. make sure that this component becomes TInplaceEdit's parent. You can also
  assign the parent during the run-time, of course - using this approach
  you can use just one TInplaceEdit control to edit numerous other components
  on your form - a great way to save system resources.

  When TInplaceEdit is first created, it makes itself invisible. At some time
  your program must position it in the proper place inside the parent,
  assign some text to it and make it visible. From this moment on, TInplaceEdit
  takes over, and allows to edit the assigned text. However, it watches
  for certain keys and events which indicate that the user finished editing
  and is attempting to leave TInplaceEdit - at this point the special event
  (published as OnGoingOff property) will be triggered. When you assign
  the handler to this event, it must be defined as follows:

  procedure InPlaceEditGoingOff(Sender: TObject; Reason: TGoingOffReason;
                                Value: String; var AllowExit: Boolean);

   "Reason" parameter allows you to find out why the event was triggered:

     goLeft   - Left Arrow key (past the first position of the editable text) or Ctrl-Left
     goRight  - Right Arrow key (past the last position of the editable text) or Ctrl-Right
     goUp     - Up Arrow key
     goDown   - Down Arrow key
     goRTab   - Tab key
     goLTab   - Shift-Tab key
     goEnter  - Return key
     goEscape - Escape key
     goPgUp   - Page Up key
     goPgDn   - Page Down key
     goHome   - Ctrl-Home key
     goEnd    - Ctrl-End key
     goNone   - some other reason (perhaps user switched to another control
                with the mouse);

   "Value" parameter contains the current text of the edit control, that is
           what user expects to be assigned to the proper element of the
           underlined parent component.

   "AllowExit" parameter allows you to decide whether user is allowed to
               exit editing at this point. If you assign False to this
               parameter - TInplaceEdit will not allow to exit from the editing
               state.

    If you don't assign False to AllowExit in this handler, TInplaceEdit will
    make itself unvisible upon return from this event.

  Procedure GoOff;

  This procedure allows you to discard currently active in-place editor
  without triggering the OnGoingOff event.


  *********************

  You may use TInplaceEdit freely at your own risk in any kind of environment.
  This component is not to be sold at any charge, and must be distributed
  along with the source code.

  While MIKSoft, Inc. claims the copyright to the original source code, this
  does not mean that you may not modify or enhance it. Just add your credits,
  and if you think you came up with some major improvement that the Delphi
  community might find useful, upload it at some Delphi site. Send us a
  copy as well to mik@cnj.digex.com
  Of course, any enhancement/modification must be released as Freeware.

  Have fun!

  A few words about MIKSoft:
  **************************

  MIKSoft, Inc. is a software development company founded in 1994.
  Currently MIKSoft offers two tools for developing demos and tutorials,
  authoring multimedia titles and for developing reliable Windows automation
  programs based on keyboard and mouse simulation.

  One is StDemo Player v2.1 and another one is ShowBasic for Windows 1.2.
  Both tools allow to develop demos/tutorials for "live" applications, i.e.
  they can control any Windows application. ShowBasic also supports
  slide-show and AVI-captured demos.

  Both tools include "smart" Recorders which allow to generate simulation
  script that can be played invariantly of Windows resolution, device driver,
  window size and position etc.

  StDemo Player is quite straightforward - you write script using
  simple scripting language and from this script you can
  (1) simulate mouse and keyboard and
  (2) show some dialogs with explanatory text, graphics, menu or
      input fields.

  ShowBasic is a much more powerful tool - the scripting language is
  full-featured Basic capable even of calling external DLLs,
  and its set of specialized functions allows to do numerous thing:
  create popup windows with formatted text and graphics
  (use even Windows API if you need something special),
  handle different events (that's how you do more than demos -
  computer based training titles), plenty of presentation features
  (transition effects, multimedia, sprites, animation), code any dialog
  boxes as templates inside your program, and much more.

  You can get more information from http://www.cnj.digex.net/~mik/

  This site offers the description of both tools, ShowBasic demo
  and StDemo Player shareware version.

  Alternatively you can get them from:

  ftp://ftp.digex.net/pub/cnj/mik/sbdemo.txt
  ftp://ftp.digex.net/pub/cnj/mik/sbdemo.zip
  ftp://ftp.digex.net/pub/cnj/mik/stdm21.txt
  ftp://ftp.digex.net/pub/cnj/mik/stdm21.zip

  MIKSoft, Inc.
  37 Landsdowne Road
  East Brunswick, NJ 08816

  voice/fax: (908) 390-8986

  mik@cnj.digex.net
  http://www.cnj.digex.net/~mik/

}

unit InplaceEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes,
  Menus, Graphics, Forms, Controls, StdCtrls;

type

  TGoingOffReason = (goNone, goLeft, goRight, goUp, goDown, goRTab, goLTab, goEnter, goEscape, goPgUp, goPgDn, goHome, goEnd);

  TGoingOffEvent = procedure(Sender: TObject; Reason: TGoingOffReason;
    Value: string; var AllowExit: Boolean) of object;

  TWheelScrollDirection = (wsUp, wsDown);

  TWheelScrollEvent = procedure(Sender: TObject; Direction: TWheelScrollDirection) of object;

  TInplaceEdit = class(TEdit)
  private
    FOnGoingOff : TGoingOffEvent;
    FRequireNotification : Boolean;
    FOnWheelScroll: TWheelScrollEvent;

  protected
    procedure CNkeydown(var Message: TWMkeydown); message CN_KEYDOWN;
    Procedure CMEnter( Var Message : TCMEnter );   message CM_ENTER;
    Procedure CMExit( Var Message : TCMExit );   message CM_EXIT;
    Procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;

    procedure TriggerGoingOffEvent(Reason: TGoingOffReason;
                                   Value: string; var AllowExit: Boolean); virtual;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    Procedure GoOff;

  published
    Property AutoSelect;
    Property AutoSize;
    Property BorderStyle;
    Property CharCase;
    Property Color;
    Property Ctl3D;
    Property Cursor;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property Height;
    Property HelpContext;
    Property HideSelection;
    Property Hint;
    Property Left;
    Property MaxLength;
    Property Name;
    Property OEMConvert;
    Property ParentColor;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property PasswordChar;
    Property PopupMenu;
    Property ReadOnly;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Tag;
    Property Text;
    Property Top;
    Property Visible;
    Property Width;
    Property OnChange;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnGoingOff: TGoingOffEvent read FOnGoingOff write FOnGoingOff;
    Property OnWheelScroll: TWheelScrollEvent read FOnWheelScroll write FOnWheelScroll;
end;

procedure Register;


implementation

{--------------------------------------------------------------------------}
procedure Register;
{--------------------------------------------------------------------------}
begin
  RegisterComponents('unSigned', [TInplaceEdit]);
end;

{--------------------------------------------------------------------------}
constructor TInplaceEdit.Create(AOwner:TComponent);
{--------------------------------------------------------------------------}
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    Visible := False;
  FRequireNotification := False;
end;

{--------------------------------------------------------------------------}
destructor TInplaceEdit.Destroy;
{--------------------------------------------------------------------------}
begin
  inherited Destroy;
end;

{--------------------------------------------------------------------------}
Procedure TInplaceEdit.GoOff;
{--------------------------------------------------------------------------}
begin
  FRequireNotification := False;
  Visible := False;
  Parent.SetFocus;
end;

{--------------------------------------------------------------------------}
procedure TInplaceEdit.TriggerGoingOffEvent(Reason: TGoingOffReason;
                Value: string; var AllowExit: Boolean);
{--------------------------------------------------------------------------}
begin
  if Assigned(FOnGoingOff) then FOnGoingOff(Self, Reason, Value, AllowExit);
end;

{--------------------------------------------------------------------------}
procedure TInplaceEdit.CNkeydown(var Message: TWMkeydown);
{--------------------------------------------------------------------------}
Var
  allowExit: Boolean;
  reason: TGoingOffReason;
  l: longint;
Begin
  allowExit := False;
  reason := goNone;
  with Message do
  begin
    Case CharCode Of
      VK_TAB :
        If ssShift In KeyDataToShiftState(KeyData) Then
          reason := goLTab
        else
          reason := goRTab;
      VK_RETURN :
          reason := goEnter;
      VK_ESCAPE :
          reason := goEscape;
      VK_UP :
          reason := goUp;
      VK_DOWN :
          reason := goDown;
      VK_LEFT :
          If ssCtrl In KeyDataToShiftState(KeyData) Then
            reason := goLeft
          Else If KeyDataToShiftState(KeyData) = [] Then
            Begin
              l := SendMessage(Handle, EM_GETSEL, 0, 0);
              if (LOWORD(l) = HIWORD(l)) and (l = 0) then
                reason := goLeft;
            End;
      VK_RIGHT :
          If ssCtrl In KeyDataToShiftState(KeyData) Then
            reason := goRight
          Else If KeyDataToShiftState(KeyData) = [] Then
            Begin
              l := SendMessage(Handle, EM_GETSEL, 0, 0);
              if (LOWORD(l) = HIWORD(l)) and (LOWORD(l) = Length(Text)) then
                reason := goRight;
            End;
      VK_HOME :
          If ssCtrl In KeyDataToShiftState(KeyData) Then
            reason := goHome;
      VK_END :
          If ssCtrl In KeyDataToShiftState(KeyData) Then
            reason := goEnd;
      VK_PRIOR :
          reason := goPgUp;
      VK_NEXT :
          reason := goPgDn;
    End;

    if reason <> goNone then
      begin
        allowExit := True;
        TriggerGoingOffEvent(reason, Text, allowExit);
      end;
    if allowExit then
      begin
        FRequireNotification := False;
        if reason <> goNone then
          Message.Result := 1; {to avoid beep on Return }
        if reason = goEscape then
          MessageBeep($ffff);
        Visible := False;
        Parent.SetFocus;
      end
    else
      begin
        if reason <> goNone then
          MessageBeep($ffff);
        exit;
      end;
  end;
end;

{--------------------------------------------------------------------------}
Procedure TInplaceEdit.CMExit( Var Message : TCMExit );
{--------------------------------------------------------------------------}
Var
  allowExit: Boolean;
Begin
  Inherited;
  allowExit:= True;
  If FRequireNotification = True Then
    TriggerGoingOffEvent(goNone, Text, allowExit);
  if allowExit then
    begin
      FRequireNotification := False;
      Visible := False;
    end
  else
    begin
      MessageBeep($ffff);
      Self.SetFocus;
    end;
End;

{--------------------------------------------------------------------------}
Procedure TInplaceEdit.CMEnter( Var Message : TCMEnter );
{--------------------------------------------------------------------------}
Begin
  Inherited;
  FRequireNotification := True;
End;

procedure TInplaceEdit.CMMouseWheel(var Message: TCMMouseWheel);
var
  ScrollCount, ScrollLines: integer;
begin
  SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
  ScrollCount := 1 * ScrollLines * Message.WheelDelta div WHEEL_DELTA;

  if Assigned(FOnWheelScroll) then
  begin
    if(ScrollCount > 0) then
      FOnWheelScroll(Self, wsUp)
    else
      FOnWheelScroll(Self, wsDown);
  end;
end;

end.

