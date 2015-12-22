unit PropertiesPage;
{ This is an component to browse the published properties of any Object
  thats inherited from TPersistent: TPropertiesPage.
  It is similar to the Delphi-Objectinspector.
  You are able to change the object properties at Runtime.

  The component based on the idea from "Adam Cutchin" who designed the
  Tf_Inspector (Last Version on 10-22-96)

  I associated other components like the InplaceEdit from "MIKSoft, Inc."
  and this compacted unit is the result of all.

  Therefor, I have some problems to free the memory after the used of
  TPropertiesPage. To solve the problem temporally, You have to call the
  FreeMemory method in the EventHandler "OnDestroy" of Your Form.
  I used Memcheck to detected memory leaks

  TPropertiesPage is an Freeware component with Source and not copyrighted.
  You can do what ever You wnat with this component.
  USE IT AT YOUR OWN RISK

  Version: 0.15; 8 Sept. 1999

  However, I hope this component can helps You.
  Please notify me if You know how to solve the FreeMemory problem
  or other unknown bugs.

  eMail: jmk@mb2.tu-chemnitz.de

  To do:
     - Property-Editor for TStrings and TFont
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, TypInfo, InplaceEdit;

type
  PropEdType = (peCategory, peItem);
  PropItemType = (piString, piCheck, piCombo);

  PPropEditData = ^TPropEditData;
  TPropEditData = record
    Name: String[30];
    Kind: PropEdType; // Item or Category
    case PropEdType of
      peItem: (Description, XmlTag, Default, Possible, Strip, Value: String[200]; Typ: PropItemType; AddSpace: Boolean);
      peCategory: (ChildShowing: Boolean);
  end;

  TEdControl = class(TInplaceEdit)
  protected
    procedure DoExit; override;
  end;

  TBtnControl = class(TSpeedButton)
  protected
    procedure Click; override;
  end;

  TLBoxControl = class(TCustomListBox)
  protected
    BtnDown: Boolean;
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TItemDetailsFunc = function(Name, Desc: String): Integer of object; stdcall;

  TPropertiesPage = class(TCustomListBox)
  private
    HalfWay: Integer;
    SplitDrag : Boolean;

    EdControl: TEdControl;
    BtnControl: TBtnControl;
    LbxControl: TLBoxControl;

    EditRect : TRect;
    DownArrow : TBitmap;
    ComboButtonBox : TRect;
    EditHit : Real;
    CBDown : Boolean;

    FOnDetailsCallback : TItemDetailsFunc;
    FModified: Boolean;
    FAutoCalcSep: Boolean; // autocalc Separator line
    procedure SetAutoCalcSep(Value: Boolean);
  protected
    { Set and Get functions for the properies }
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;  Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateItem(AItem: TPropEditData);
    procedure ClearItems;
    procedure FreeMemory;
  published
    { publish the TListBox properties }
    property AutoCalcSep: Boolean read FAutoCalcSep write SetAutoCalcSep;
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style default lbOwnerDrawFixed;
    property TabOrder;
    property Visible;
    property Width;
    { pusblish the TListBox events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property Modified: Boolean read FModified write FModified;
    property OnDetailsCallback : TItemDetailsFunc read FOnDetailsCallback write FOnDetailsCallback;
  end;

procedure Register;

implementation

function StringUtils_SplitString(const StringToSplit: String; const Delimiter: Char): TStrings;
var
  ListOfStrings: TStrings;
begin
  try
     // http://stackoverflow.com/questions/2625707/split-a-string-into-an-array-of-strings-based-on-a-delimiter
     ListOfStrings := TStringList.Create;
     ListOfStrings.Clear;
     ListOfStrings.Delimiter       := Delimiter;
     ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
     ListOfStrings.DelimitedText   := StringToSplit;

     Result := ListOfStrings;
  except

  end;
end;

{ TEdControl }
procedure TEdControl.DoExit;
Var
  PPED : PPropEditData;             { property to what was entered into the edit box. }

begin
  If Modified then
  Begin
    with (Parent as TPropertiesPage) do
      PPED := Pointer(Items.Objects[ItemIndex]);
    (Parent as TPropertiesPage).Modified := Self.Modified;

    If PPED^.Kind = peItem then
      Case PPED^.Typ of
        piCheck:
        Begin

        End;
        piCombo:
        Begin

        End;
        piString:
        Begin
          PPED^.Value := Text;
        End;
      End
    Else
      MessageBeep(0);
  end;
End;

{ TBtnControl }
procedure TBtnControl.Click;
begin
  (Parent as TPropertiesPage).DblClick;
end;

{ TLBoxControl }
procedure TLBoxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PPED: PPropEditData;
  Modified: Boolean;
begin
  if not btnDown then
  Begin
    SendMessage(Handle, wm_LButtonDown, mk_Lbutton, y shl 16 + x);
    sendMessage(Handle, wm_LButtonUp, mk_Lbutton, y shl 16 + x);
  End;

  with (Parent as TPropertiesPage) do
    PPED := Pointer(Items.Objects[ItemIndex]);

  If PPED^.Kind = peItem then
  Begin
    Modified := (PPED^.Value <> Items[ItemIndex]);
    PPED^.Value := Items[ItemIndex];

    (Parent as TPropertiesPage).SetFocus;
    (Parent as TPropertiesPage).Modified := Modified;
  End;
end;

procedure TLBoxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If (X >= ClientRect.left) and (X <= ClientRect.Right) and
     (y >= ClientRect.Top) and (Y <= ClientRect.Bottom) then
  Begin
    BtnDown := True;
  End;
end;

procedure TLBoxControl.DoExit;
begin
  Visible := False;
  (Parent as TPropertiesPage).CBDown := False;
  (Parent as TPropertiesPage).SetFocus;
end;

{ TPropertiesPage }
procedure TPropertiesPage.CreateParams;
begin
  inherited CreateParams(Params); { call the inherited first }
//  with Params do Style := Style or WS_CAPTION;// or WS_SIZEBOX; { add a style flag }
  with Params do ExStyle := ExStyle or WS_EX_TOOLWINDOW;
  FModified := False;
end;

constructor TPropertiesPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoCalcSep := False;
  
  ItemHeight := 20;
  Color := clBtnFace;
  Style := lbOwnerDrawFixed;
  { edit control }
  EdControl := TEdControl.Create(Self);
  EdControl.BorderStyle := bsNone;
  EdControl.Parent := Self;
  EdControl.Visible := False;
  { Button control }
  BtnControl := TBtnControl.Create(Self);
  BtnControl.Parent := Self;
  BtnControl.Visible := False;
  { Listbox control }
  LbxControl := TLBoxControl.Create(Self);
  LbxControl.Ctl3D := False;
  LbxControl.ItemHeight := 20;
  LbxControl.Parent := Self;
  LbxControl.Visible := False;
  { Down arrow }
  DownArrow := TBitmap.Create;
  DownArrow.Width := 7;
  DownArrow.Height := 4;
  DownArrow.canvas.Brush.Color := clBtnFace;
  DownArrow.canvas.Pen.Color := clBtnface;
  DownArrow.Canvas.Rectangle(0,0,7,4);
  DownArrow.Canvas.Pen.Width := 0;
  DownArrow.Canvas.pen.Color := clBlack;
  DownArrow.Canvas.MoveTo(0, 0);
  DownArrow.Canvas.LineTo(7, 0);
  DownArrow.Canvas.MoveTo(1, 1);
  DownArrow.Canvas.LineTo(6, 1);
  DownArrow.Canvas.MoveTo(2, 2);
  DownArrow.Canvas.LineTo(5, 2);
  DownArrow.Canvas.Pixels[3, 3] := clBlack;
end;

destructor TPropertiesPage.Destroy;
begin
  DownArrow.Free;
  BtnControl.Free;
  EdControl.Free;
  LbxControl.Free;
  inherited Destroy;
end;

procedure TPropertiesPage.FreeMemory;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    FreeMem(Pointer(Items.Objects[i]), SizeOf(TPropEditData));
end;

procedure TPropertiesPage.SetAutoCalcSep(Value: Boolean);
begin
  if FAutoCalcSep <> Value then
    FAutoCalcSep := Value;
end;

procedure TPropertiesPage.CreateItem(AItem: TPropEditData);
var
  PPED : PPropEditData;
begin
  if EdControl.Focused then
    SetFocus;
  BtnControl.Visible := false;
  Visible := True;

  try
    HalfWay := 0;
    if not FAutoCalcSep then
      HalfWay := Width div 2;

    GetMem(PPED, SizeOf(TPropEditData));
    PPED^.Name := Copy(AItem.Name, 1, Length(AItem.Name));
    PPED^.Kind := AItem.Kind;

    if(AItem.Kind = peCategory) then
      PPED^.ChildShowing := True
    else
    begin
      PPED^.Description := Copy(AItem.Description, 1, Length(AItem.Description));
      PPED^.XmlTag := Copy(AItem.XmlTag, 1, Length(AItem.XmlTag));
      PPED^.Default := Copy(AItem.Default, 1, Length(AItem.Default));
      PPED^.Value := Copy(AItem.Value, 1, Length(AItem.Value));
      PPED^.Possible := Copy(AItem.Possible, 1, Length(AItem.Possible));
      PPED^.Strip := Copy(AItem.Strip, 1, Length(AItem.Strip));
      PPED^.Typ := AItem.Typ;
      PPED^.AddSpace := AItem.AddSpace;
    end;

    if FAutoCalcSep then
      if Canvas.TextWidth(PPED^.Name)+10 > HalfWay then
        HalfWay := Canvas.TextWidth(PPED^.Name)+10;
    Items.AddObject(AItem.Name, TObject(PPED));
  finally

  end;
end;

procedure TPropertiesPage.ClearItems;
begin
  try
    if EdControl.Focused then
      SetFocus;
    BtnControl.Visible := false;
    FreeMemory;
    Clear;
    Visible := True;
  finally

  end;
end;

procedure TPropertiesPage.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  EdString, RString: string;
  PEditData: PPropEditData;
  inStr: String;
  SRect: TRect;
  DRect: TRect;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    PEditData := Pointer(Items.Objects[Index]);
    HideCaret(Handle);
    inStr := '   ';

    if PEditData^.Kind = peCategory then
    begin
        inStr := Copy(inStr, 1, Length(inStr) - 2);
        if PEditData^.ChildShowing then
          inStr := '- '
        else
          inStr := '+ ';
    end;

    { draw the item }
    with Canvas do
    begin
      Brush.Color := clWhite;  {Won't use Windows Selected color.}
      SRect := Rect;

      if(PEditData.Kind = peCategory) then
      begin
        state := state - [odSelected];
        Brush.Color := Self.Color;
      end;

      If odSelected in state then
      begin
        Brush.Color := clHighlight;
        SRect.Right := HalfWay;
      end;
      FillRect(SRect);
      if (odSelected in State) then
        Pen.Color := clWhite
      else {for 3D look}
        Pen.Color := self.Color;

      Pen.Mode := pmCopy;
      Pen.Width := 1;
      Pen.Style := psSolid;
      MoveTo(rect.Left, Rect.Bottom-1);
      LineTo(rect.Right, Rect.Bottom-1);
      if (odSelected in State) then
      begin
        //Pen.Color := clBtnHighLight;
        //MoveTo(HalfWay + 2, Rect.Top+1);
        //LineTo(HalfWay + 2, Rect.Bottom);

        Pen.Color := clBtnShadow;
        MoveTo(HalfWay+1, Rect.Top+2);
        LineTo(HalfWay+1, Rect.Bottom);

        MoveTo(Rect.Right, Rect.Top);
        LineTo(Rect.Left, Rect.Top);
        LineTo(Rect.Left, Rect.Bottom);

        //Pen.Color := clBlue;
        //MoveTo(Rect.Right - 1, Rect.Top + 1);
        //LineTo(Rect.Left + 1, Rect.Top + 1);
        //LineTo(Rect.Left + 1, Rect.Bottom - 1);
      end else
      begin
        Pen.Color := clBtnShadow;
        MoveTo(HalfWay, Rect.Top); //Top+1
        LineTo(HalfWay, Rect.Bottom);

        Pen.Color := clBtnHighLight;
        MoveTo(HalfWay + 1, Rect.Top); // Top+1
        LineTo(HalfWay + 1, Rect.Bottom);
      end;

      SRect.Left := Rect.Left;
      SRect.Right := HalfWay;
      SRect.Top := Rect.Top+1;
      SRect.Bottom := Rect.Bottom-1;
      if (odSelected in State) then
        Font.Color := clWhite
      else
        Font.Color := clWindowText;
      if(PEditData.Kind = peCategory) then
        Font.Style := Font.Style + [fsBold];

      If (odSelected in State) then
      Begin
        Inc(SRect.Left);
        Inc(SRect.Top);
        Inc(SRect.Right);
      End;

      // Left column text
      setBkMode(Canvas.Handle, Transparent);
      TextRect(SRect, SRect.Left, SRect.Top, inStr + PEditData^.Name);

      Pen.Color := clBtnFace;
      Font.Color := clWindowText;

      try
        if PEditData^.Kind = peCategory then
          RString := '';

        if PEditData^.Kind = peItem then
          RString := PEditData^.Value;
      except
        RString := '** Exception **';
      End;

  //Canvas.Font.Color := clRed; // Text color on the right side (property value-str)
  if(PEditData^.Kind = peCategory) then
    Font.Style := Font.Style + [fsBold];

  // Mark non-default values as bold (like changed)
  if(PEditData.Kind = peItem) then
    if(PEditData^.Value <> PEditData^.Default) then
      Font.Style := Font.Style + [fsBold];

  SRect := ItemRect(ItemIndex);
  if odSelected in state then
    if PEditData^.Kind = peItem then      { Enumeration - Simulate List box. }
      if PEditData^.Typ = piCombo then
      begin
        ComboButtonBox := SRect;
        SRect.Right := SRect.Right - ABS(SRect.Bottom - SRect.Top);
        Inc(ComboButtonBox.Top, 2);
        ComboButtonBox.Left := SRect.Right + 1;
        DrawButtonFace(Canvas, ComboButtonBox, 3, bsNew, false, false, false);

        DRect.Left := ComboButtonBox.Left +
          ((ComboButtonBox.Right - ComboButtonBox.Left) DIV 2) - (7 DIV 2);
        DRect.Right := DRect.Left + DownArrow.Width;
        DRect.Top := ComboButtonBox.Top +
          ((ComboButtonBox.Bottom - ComboButtonBox.Top) DIV 2) - (4 DIV 2);
        DRect.Bottom := DRect.Top + DownArrow.Height;

        Canvas.CopyRect(DRect, DownArrow.Canvas, DownArrow.Canvas.ClipRect);

        BtnControl.visible := False;
      End;

      if (not EdControl.Focused) and (odSelected in State) then
      With Canvas do
      Begin
        EdControl.Visible := False;
        EditRect := ItemRect(ItemIndex);
        EditRect.Left := HalfWay + 2;
        Brush.Color := clWhite;
        Inc(EditRect.Top, 2);
        if PEditData^.Kind = peItem then
        if PEditData^.Typ = piCombo then
          EditRect.Right := EditRect.Right - ABS(EditRect.Bottom - EditRect.Top);
        if (PEditData.Kind = peItem) and True {(PInfo^.PropType^.Kind = tkClass)} then
        {if Assigned(Object2) then }
        begin
          if False then
          begin
            BtnControl.Width := EditRect.Bottom - EditRect.Top;
            EditRect.Right := EditRect.Right - ABS(EditRect.Bottom - EditRect.Top);
            BtnControl.Left := Editrect.Right + 1;
            BtnControl.Top := EditRect.Top;
            BtnControl.Height := EditRect.Bottom - EditRect.Top;
            BtnControl.Visible := true;
          End else
            BtnControl.Visible := False;
        End else
          BtnControl.Visible := False;
        Fillrect(EditRect);
        setBkColor(Canvas.Handle, GetSysColor(color_HighLight));
        setBkMode(Canvas.Handle, Opaque);
        Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
        EdString := RString;
        If (PEditData.Kind = peCategory) and True
          {(PInfo^.PropType^.kind in [tkSet, tkEnumeration, tkClass])} then
          EdControl.ReadOnly := True
        else
          EdControl.ReadOnly := False;

        while (TextWidth(EdString) > (EditRect.Right - EditRect.Left)) and
          (EdString <> '')  do
          Delete(EdString, 1, 1);
        TextOut(EditRect.Left + 2, EditRect.Top+1, edString);
        DestroyCaret;
        CreateCaret(Handle, 0, 1, (EditRect.Bottom - Editrect.Top) - 2);
        SetCaretPos(EditRect.Left + TextWidth(EdString), EditRect.Top + 1);
        ShowCaret(Handle);
        EdControl.Text := RString;
      end else
      begin
        if (odSelected in state) and CBDown then
        Begin
          Canvas.Brush.Color := clWhite;
          Canvas.FillRect(EditRect);
          Font.Color := clWindowText;
          If EdControl.Focused then
            RString := EdControl.Text;
          Canvas.TextOut(HalfWay + 2, Rect.Top+2, RString);
        end else;
        Canvas.TextOut(HalfWay + 3, Rect.Top+1, RString);
      End;
      if (odFocused in State) and (odSelected in State) then
        Canvas.DrawFocusRect(Rect);

      If BtnControl.Visible then
        BtnControl.Refresh;
    end;
  end;
end;

procedure TPropertiesPage.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  PPED: PPropEditData;
  charDelimiter : Char;
  strArray  : TStrings;
  Temp: String;
  I: Integer;

begin
  PPED := Pointer(Items.Objects[ItemIndex]);
  If Abs(x - Halfway) <= 3 then
    SplitDrag := True
  else
  begin
    If (X >= EditRect.left) and (X <= EditRect.Right) and
       (y >= EditRect.Top) and (Y <= EditRect.Bottom) then
    Begin
      P.X := x;
      p.y := y;
      Cursor := crDefault;
      EdControl.left := EditRect.left;
      EdControl.Top := EditRect.Top;
      EdControl.Width := EditRect.Right - EditRect.Left;
      EdControl.Height := EditRect.Bottom - EditRect.Top;
      Edithit := Time;
      EdControl.Visible := True;
      MapWindowPoints(Handle, EdControl.Handle, P, 1);
      PostMessage(EdControl.Handle, WM_LButtonDown, MK_LButton, P.Y shl 16 + P.X);
      FOnDetailsCallback(PPED^.Name, PPED^.Description);
    End Else
    If (X >= ComboButtonBox.left) and (X <= ComboButtonBox.Right) and
      (y >= ComboButtonBox.Top) and (Y <= ComboButtonBox.Bottom) and (not CBDown) then
    Begin
      LbxControl.Visible := True;
      EdControl.Visible := False;
      BtnControl.Visible := False;
      CBDown := True;
      LbxControl.Items.Clear;
      FOnDetailsCallback(PPED^.Name, PPED^.Description);

      LbxControl.Top := EditRect.Bottom;
      LbxControl.Left := EditRect.Left;
      LbxControl.Width := ComboButtonBox.Right-EditRect.Left;

      charDelimiter := '|';
      Temp := PPED^.Possible;
      strArray := StringUtils_SplitString(Temp, charDelimiter);

      for I := 0 to strArray.Count - 1 do
        LbxControl.Items.Add(strArray[I]);

      if LbxControl.Items.Count > 6 then
        LbxControl.Height := 4 + LbxControl.ItemHeight *  6
      else
        LbxControl.Height := 4 + LbxControl.ItemHeight * LbxControl.Items.Count;
     LbxControl.SetFocus;
    End;

    if(x < Halfway) then
      FOnDetailsCallback(PPED^.Name, PPED^.Description);
  end;
end;

procedure TPropertiesPage.MouseMove(Shift: TShiftState; X, Y: Integer);
{ handles dragging the split in the two panes. }
Var
  TempR : TRect;
begin
  If SplitDrag then
  Begin
    If X > ClientWidth - 20 then
      X := ClientWidth - 20;
    If X < 20 then
      X := 20;
    If Halfway < x then
    Begin
      TempR.Left := Halfway;
      TempR.Right := ClientWidth;
      TempR.Top := 0;
      TempR.Bottom := ClientHeight;
    End Else
    Begin
      TempR.Left := X;
      TempR.Right := ClientWidth;
      TempR.Top := 0;
      TempR.Bottom := ClientHeight;
    End;
    HalfWay := x;
    InvalidateRect(Handle, @tempR, false);
    InvalidateRect(Handle, @EditRect, False);
    PostMessage(Handle, WM_Paint, 0, 0);
    Exit;
  End;
  If Abs(x - HalfWay) <= 3 then
    Cursor := crHSplit
  Else
  If (X >= EditRect.left) and (X <= EditRect.Right) and
     (y >= EditRect.Top) and (Y <= EditRect.Bottom) then
    Cursor := crIBeam else Cursor := crDefault;
End;

procedure TPropertiesPage.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SplitDrag := False;
end;

procedure TPropertiesPage.DblClick;
var
  PPED : PPropEditData;
  P : Tpoint;

begin
  GetCursorPos(P);
  MapWindowPoints(0, Handle, P, 1);
  PPED := Pointer(Items.Objects[ItemIndex]);
  If P.X < halfway then
  Begin
    { Handle clicks left of the break. }
  End
  Else
  Begin
    { Handle clicks right of the divide. }
  End;
  SetFocus;
  Refresh;
end;

procedure TPropertiesPage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  If (Key = VK_RETURN) or (Key = VK_SPACE) then
  begin
    Click;
    Key := 0;
    Exit;
  end;
  if not (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) then
  begin
    EdControl.left := EditRect.left;
    EdControl.Top := EditRect.Top;
    EdControl.Width := EditRect.Right - EditRect.Left;
    EdControl.Height := EditRect.Bottom - EditRect.Top;
    EdControl.Visible := True;
    EdControl.SetFocus;
    PostMessage(EdControl.Handle, WM_KeyDown, key, 0);
    Key := 0;
  End;
end;

procedure Register;
begin
  RegisterComponents('unSigned', [TPropertiesPage]);
end;

Begin

end.
