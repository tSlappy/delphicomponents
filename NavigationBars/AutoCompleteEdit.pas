unit AutoCompleteEdit;

interface

uses
  Windows, Classes, StdCtrls, SysUtils, StrUtils, UtilityFunctions,
  DropDownMember,
  Generics.Collections, Messages, Graphics, Controls, Math, Forms;

{$R 'Resources\RADInstaller.res' 'Resources\RADInstaller.rc'}

const
  MSG_HIDEWORDLIST = WM_USER + 222;

  // Drawing constants
  coVisibleItems  = 10;
  coImageWidth    = 16;
  coImageHeight   = 16;
  coImagePadding  = 1;

type

  TListHideEvent = procedure(Sender: TObject) of object;

  TAutoCompleteMan = class
  private
    FMembers: TList<TDropDownMember>;

  public
    constructor Create;
    destructor Destroy; override;

    function IsRecognized(ALabel: string; Members: TList<TDropDownMember>): Boolean;

    procedure AddMember(const member: TDropDownMember);
    procedure ClearData;
    function GetList: TList<TDropDownMember>;

    property Members: TList<TDropDownMember> read FMembers;
  end;

  TAutoCompleteEdit = class(TEdit)
  private
    FItemsList: TListBox;
    FPopupForm: TForm;
    FWordListHeight: Integer;
    FWordListWidth: Integer;
    FAutoCompleteMan: TAutoCompleteMan;

    FSelected: TDropDownMember;
    FSaveChangeEvent: TNotifyEvent;
    FCueText: String;

    FBitmapsInnoSetup: Array of TBitmap;
    FBitmapsCount: Integer;

    FCanvas: TCanvas;
    FOnListHide: TListHideEvent;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure HandleWordListLostFocus(ASender: TObject);
    procedure HandleWordListKeyPress(Sender: TObject; var Key: Char);
    procedure HandleWordListKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleWordListClick(ASender: TObject);
    procedure HandleHideWordList(var AMsg); overload; message MSG_HIDEWORDLIST;
    procedure HandleHideWordList; overload;
    procedure DrawTransparentBmp(Cnv: TCanvas; x,y: Integer; Bmp: TBitmap; clTransparent: TColor);

    procedure RegainFocus;

  protected
    procedure ShowMembersList(AMembers: TList<TDropDownMember>);
    procedure HideWordList;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;

    procedure OnListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    property Canvas: TCanvas read FCanvas;

  public
    constructor Create(AOwner: TComponent); overload;
    constructor Create(AOwner: TComponent; cueText: String); overload;
    destructor Destroy; override;

    procedure AddMember(const member: TDropDownMember);
    procedure ClearData;
    procedure ShowAllItems;

  published
   property Anchors;
   property AutoSelect;
   property AutoSize;
   property BevelEdges;
   property BevelInner;
   property BevelKind default bkNone;
   property BevelOuter;
   property BevelWidth;
   property BiDiMode;
   property BorderStyle;
   property CharCase;
   property Color;
   property Constraints;
   property Ctl3D;
   property DragCursor;
   property DragKind;
   property DragMode;
   property Enabled;
   property Font;
   property HideSelection;
   property ImeMode;
   property ImeName;
   property MaxLength;
   property OEMConvert;
   property ParentBiDiMode;
   property ParentColor;
   property ParentCtl3D;
   property ParentFont;
   property ParentShowHint;
   property PasswordChar;
   property PopupMenu;
   property ReadOnly;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Text;
   property Visible;
   property OnChange;
   property OnClick;
   property OnContextPopup;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMouseActivate;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnStartDock;
   property OnStartDrag;

   property OnListHide: TListHideEvent read FOnListHide write FOnListHide;
  end;


implementation

{ TAutocompleteMan }

procedure TAutoCompleteMan.AddMember(const member: TDropDownMember);
begin
  FMembers.Add(member);
end;

procedure TAutoCompleteMan.ClearData;
begin
  FMembers.Clear;
end;

function TAutoCompleteMan.GetList: TList<TDropDownMember>;
begin
  Result := FMembers;
end;

constructor TAutoCompleteMan.Create;
begin
  FMembers := TList<TDropDownMember>.Create;
  //!TStringList(FMembers).Duplicates := dupIgnore;
end;

destructor TAutoCompleteMan.Destroy;
begin
  FMembers.Free;
  inherited;
end;

function TAutoCompleteMan.IsRecognized(ALabel: string; Members: TList<TDropDownMember>): Boolean;
var
  i: Integer;
begin
  Result := False;
  ALabel := UpperCase(ALabel);
  for i := 0 to FMembers.Count - 1 do
  begin
    if ((ALabel = '') Or (StringUtils_Contains_IC(FMembers[i].Labell, ALabel))) then
    begin
      Result := True;
      Members.Add(FMembers[i]);
    end;
  end;
end;

{ TAutoCompleteEdit }

procedure TAutoCompleteEdit.Change;
var
  Members: TList<TDropDownMember>;

begin
  inherited;

  try
    Members := TList<TDropDownMember>.Create;
    if(Assigned(FSelected)) then
      FSelected := nil;

    if FAutoCompleteMan.IsRecognized(Self.Text, Members) then
      ShowMembersList(Members)
    else
      HideWordList;
  finally
    Members.Free;
  end;
end;

procedure TAutoCompleteEdit.HandleHideWordList(var AMsg);
begin
  HandleHideWordList;
end;

constructor TAutoCompleteEdit.Create(AOwner: TComponent);
var
  I: Integer;
  SourceRect, DestRect: TRect;
  BitmapI, BitmapN: TBitmap;

begin
  inherited;

  FAutoCompleteMan := TAutoCompleteMan.Create;

  FSelected := nil;
  FSaveChangeEvent := nil;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  // Create images for items
  BitmapI := TBitmap.Create;
  BitmapN := TBitmap.Create;
  // Picture dimensions for Inno Setup and NSIS must be the same!!!

  BitmapI.LoadFromResourceName(hInstance, 'InnoSetupIntelliSenseIcons');
  BitmapN.LoadFromResourceName(hInstance, 'NsisIntelliSenseIcons');

  FBitmapsCount := Round(BitmapI.Width / coImageWidth);
  SetLength(FBitmapsInnoSetup, FBitmapsCount);

  DestRect.Left := 0;
  DestRect.Right := coImageWidth;
  DestRect.Top := 0;
  DestRect.Bottom := coImageHeight;

  SourceRect.Top := 0;
  SourceRect.Bottom := coImageHeight;

  for i := 0 to FBitmapsCount - 1 do
  begin
    FBitmapsInnoSetup[i] := TBitmap.Create;
    FBitmapsInnoSetup[i].SetSize(coImageWidth, coImageHeight);

    SourceRect.Left := i * coImageWidth;
    SourceRect.Right := SourceRect.Left + coImageWidth;

    FBitmapsInnoSetup[i].Canvas.CopyRect(DestRect, BitmapI.Canvas, SourceRect);
  end;
  BitmapI.Free;
  BitmapN.Free;
end;

constructor TAutoCompleteEdit.Create(AOwner: TComponent; cueText: String);
begin
  Create(AOwner);
  Self.FCueText := cueText;
end;

destructor TAutoCompleteEdit.Destroy;
var
  I: Integer;

begin
  FAutoCompleteMan.Free;

  FCanvas.Free;

  for i := 0 to FBitmapsCount - 1 do
  begin
    FBitmapsInnoSetup[i].Free;
  end;

  inherited;
end;

procedure TAutoCompleteEdit.DoExit;
begin
  //if Assigned(FItemsList) and FItemsList.Visible and not FItemsList.Focused then
  //  HideWordList;
  inherited;
end;

procedure TAutoCompleteEdit.HandleHideWordList;
begin
  if(FItemsList <> nil) then
  begin
    FItemsList.Clear;
    FItemsList.Free;
    FItemsList := nil;
    //FPopupForm.Hide;
    FPopupForm.Free;
    FPopupForm := nil;
  end;

  if Assigned(FOnListHide) then
    FOnListHide(Self)
end;

procedure TAutoCompleteEdit.HandleWordListKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) and (FItemsList.ItemIndex = 0) then
    RegainFocus;
end;

procedure TAutoCompleteEdit.HandleWordListClick(ASender: TObject);
var
  SelIndex: Integer;
  Member: TDropDownMember;

begin
  SelIndex := FItemsList.ItemIndex;
  if(SelIndex > - 1)then
  begin
     Member := TDropDownMember(FItemsList.Items.Objects[SelIndex]);
     FSaveChangeEvent := Self.OnChange;
     Self.OnChange := nil;
     Self.Text := Member.Labell;
     Self.SetFocus;
     Self.SelStart := 0;
     Self.SelLength := Length(Self.Text);

     // Set selected data
     FSelected := Member;
     HideWordList;
     Self.OnChange := FSaveChangeEvent;
  end;
end;

procedure TAutoCompleteEdit.HandleWordListKeyPress(Sender: TObject; var Key: Char);
var
  SelIndex: Integer;
  Member: TDropDownMember;

begin
  SelIndex := FItemsList.ItemIndex;
  case Key of
    #13: begin
           Key := #0;
           Member := TDropDownMember(FItemsList.Items.Objects[SelIndex]);
           FSaveChangeEvent := Self.OnChange;
           Self.OnChange := nil;
           Self.Text := Member.Labell;
           Self.SetFocus;
           Self.SelStart := 0;
           Self.SelLength := Length(Self.Text);

           // Set selected data
           FSelected := Member;
           HideWordList;
           Self.OnChange := FSaveChangeEvent;
         end;
    #27: begin
           RegainFocus;
           HideWordList;
         end;
    else begin
      RegainFocus;
    end;
  end;
end;

procedure TAutoCompleteEdit.HandleWordListLostFocus(ASender: TObject);
begin
  if not Self.Focused then
    HideWordList;
end;

procedure TAutoCompleteEdit.HideWordList;
begin
  if(Self.Text <> '#') then
    PostMessage(Self.Handle, MSG_HIDEWORDLIST, 0, 0);
end;

procedure TAutoCompleteEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    HideWordList
  else if (Key = VK_DOWN) and Assigned(FItemsList) and FItemsList.Visible then
  begin
    FItemsList.SetFocus;
    if FItemsList.ItemIndex < 0 then
      FItemsList.ItemIndex := 0;
  end;

  inherited;
end;

procedure TAutoCompleteEdit.RegainFocus;
begin
  Self.SetFocus;
  Self.SelStart := 0;
  Self.SelLength := Length(Self.Text);
end;

procedure TAutoCompleteEdit.ShowMembersList(AMembers: TList<TDropDownMember>);
var
  I: Integer;
  FormPoint: TPoint;

begin
  if (FItemsList = nil) then
  begin
    FPopupForm := TForm.Create(Self);
    FItemsList := TListBox.Create(FPopupForm);
    FItemsList.ParentCtl3D := False;
    FItemsList.Ctl3D := False;
    FItemsList.Parent := FPopupForm;
    FItemsList.TabStop := False;
    FItemsList.OnExit := HandleWordListLostFocus;
    FItemsList.OnKeyPress := HandleWordListKeyPress;
    FItemsList.OnKeyDown := HandleWordListKeyDown;
    //FItemsList.OnClick := HandleWordListClick; // This event is causing troubles: auto accepting first item in list as selected
    FItemsList.OnDblClick := HandleWordListClick;
    FItemsList.Style := lbOwnerDrawFixed;
    FItemsList.OnDrawItem := OnListBoxDrawItem;
    FItemsList.ItemHeight := (2 * coImagePadding) + coImageHeight;
  end;

  FItemsList.Items.Clear;
  for I := 0 to AMembers.Count - 1 do
  begin
    FItemsList.Items.AddObject(AMembers[i].Labell, TObject(AMembers[i]));
  end;

  // Automatically set dimensions
  FWordListWidth := Width + (2 * coImagePadding) + coImageHeight;
  FWordListHeight := Min(Integer(FItemsList.ItemHeight * coVisibleItems), Integer(FItemsList.ItemHeight * FItemsList.Items.Count));
  FItemsList.SetBounds(0, 0, FWordListWidth, FWordListHeight);

  FormPoint := Self.ClientToScreen(Point(0, 0));
  FPopupForm.SetBounds(FormPoint.X, FormPoint.Y + Self.Height, FWordListWidth, FWordListHeight);
  FPopupForm.BorderStyle := bsNone;
  FPopupForm.ClientWidth := FWordListWidth;
  FPopupForm.ClientHeight := FWordListHeight;

  FPopupForm.FormStyle := fsStayOnTop;
  FPopupForm.Show;
  FItemsList.Show;
  Self.SetFocus;
end;

procedure TAutoCompleteEdit.AddMember(const member: TDropDownMember);
begin
  FAutoCompleteMan.AddMember(member);
end;

procedure TAutoCompleteEdit.ClearData;
begin
  FAutoCompleteMan.ClearData;
end;

procedure TAutoCompleteEdit.ShowAllItems;
begin
  // If the list is visible hide it
  if(FItemsList <> nil) then
  begin
    if(FItemsList.Visible) then
    begin
      PostMessage(Self.Handle, MSG_HIDEWORDLIST, 0, 0);
      Exit;
    end;
  end;

  if(Self.Text = '') then
    Self.Text := '#';

  Self.Text := '';
end;

////////////////////////////////////////////////////////////////////////////////
// Drawing

procedure TAutoCompleteEdit.OnListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  TempRect: TRect;
  Member: TDropDownMember;
  Canvas: TCanvas;

begin
  Member := TDropDownMember(TListBox(Control).Items.Objects[Index]);
  Canvas := TListBox(Control).Canvas;

  Canvas.FillRect(Rect);
  Assert(Member <> nil, 'Drawing when Member = nil!!!');
  DrawTransparentBmp(Canvas, Rect.Left + coImagePadding, Rect.Top + coImagePadding, FBitmapsInnoSetup[Member.Glyph], clFuchsia);

  TempRect := Rect;
  TempRect.Left := Rect.Left + (2 * coImagePadding) + coImageWidth;

  Canvas.Font.Name := Self.Font.Name;
  Canvas.Font.Size := Self.Font.Size;
  Canvas.Font.Color := Self.Font.Color;

  if(FONTATTR_BOLD in Member.FontAttr) then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  if(FONTATTR_ITALIC in Member.FontAttr) then
    Canvas.Font.Style := Canvas.Font.Style + [fsItalic];

  if(FONTATTR_UNDERLINE in Member.FontAttr) then
    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];

  if(FONTATTR_GRAY in Member.FontAttr) then
    Canvas.Font.Color := clLtGray;

  Canvas.TextOut(TempRect.Left, TempRect.Top, TListBox(Control).Items[Index]);
end;

procedure TAutoCompleteEdit.Paint;
var
 Rect, TempRect: TRect;
 I: Integer;
 S: String;

begin
  Rect := ClientRect;
  Canvas.FillRect(Rect);
  TempRect := Rect;
  TempRect.Left := Rect.Left + (2 * coImagePadding) + coImageWidth;

  if(FSelected <> nil) then
  begin
    DrawTransparentBmp(Canvas, Rect.Left + coImagePadding, Rect.Top + coImagePadding, FBitmapsInnoSetup[FSelected.Glyph], clFuchsia);

    if(FONTATTR_BOLD in FSelected.FontAttr) then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];

    if(FONTATTR_ITALIC in FSelected.FontAttr) then
      Canvas.Font.Style := Canvas.Font.Style + [fsItalic];

    if(FONTATTR_UNDERLINE in FSelected.FontAttr) then
      Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];

    if(FONTATTR_GRAY in FSelected.FontAttr) then
      Canvas.Font.Color := clLtGray;

    Canvas.TextOut(TempRect.Left, TempRect.Top, FSelected.Labell);
  end
  else
  begin
    Canvas.Font.Style := Canvas.Font.Style - [fsBold, fsItalic, fsUnderline];

    DrawTransparentBmp(Canvas, Rect.Left + coImagePadding, Rect.Top + coImagePadding, FBitmapsInnoSetup[19], clFuchsia);

    Canvas.TextOut(Rect.Left, Rect.Top, Self.Text);
  end;
end;

procedure TAutoCompleteEdit.PaintWindow(DC: HDC);
begin
 FCanvas.Lock;
 try
   FCanvas.Handle := DC;
   try
     TControlCanvas(FCanvas).UpdateTextFlags;
     Paint;
   finally
     FCanvas.Handle := 0;
   end;
 finally
   FCanvas.Unlock;
 end;
end;

procedure TAutoCompleteEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;

  Invalidate;

  SendMessage(Self.Handle, EM_SETSEL, Length(Self.Text), 0);
end;

procedure TAutoCompleteEdit.WMPaint(var Message: TWMPaint);
begin
  // Do not use ownerdraw if nothing is selected
  if(FSelected <> nil) then
    ControlState := ControlState + [csCustomPaint];
  inherited;
  if(FSelected <> nil) then
    ControlState := ControlState - [csCustomPaint];
end;

procedure TAutoCompleteEdit.DrawTransparentBmp(Cnv: TCanvas; x,y: Integer; Bmp: TBitmap; clTransparent: TColor);
var
  bmpXOR, bmpAND, bmpINVAND, bmpTarget: TBitmap;
  oldcol: Longint;
begin
  try
    bmpAND := TBitmap.Create;
    bmpAND.Width := Bmp.Width;
    bmpAND.Height := Bmp.Height;
    bmpAND.Monochrome := True;
    oldcol := SetBkColor(Bmp.Canvas.Handle, ColorToRGB(clTransparent));
    BitBlt(bmpAND.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, Bmp.Canvas.Handle, 0,0, SRCCOPY);
    SetBkColor(Bmp.Canvas.Handle, oldcol);

    bmpINVAND := TBitmap.Create;
    bmpINVAND.Width := Bmp.Width;
    bmpINVAND.Height := Bmp.Height;
    bmpINVAND.Monochrome := True;
    BitBlt(bmpINVAND.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, bmpAND.Canvas.Handle, 0,0, NOTSRCCOPY);

    bmpXOR := TBitmap.Create;
    bmpXOR.Width := Bmp.Width;
    bmpXOR.Height := Bmp.Height;
    BitBlt(bmpXOR.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(bmpXOR.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, bmpINVAND.Canvas.Handle, 0,0, SRCAND);

    bmpTarget := TBitmap.Create;
    bmpTarget.Width := Bmp.Width;
    bmpTarget.Height := Bmp.Height;
    BitBlt(bmpTarget.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, Cnv.Handle, x,y, SRCCOPY);
    BitBlt(bmpTarget.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, bmpAND.Canvas.Handle, 0,0, SRCAND);
    BitBlt(bmpTarget.Canvas.Handle, 0,0,Bmp.Width,Bmp.Height, bmpXOR.Canvas.Handle, 0,0, SRCINVERT);
    BitBlt(Cnv.Handle, x,y,Bmp.Width,Bmp.Height, bmpTarget.Canvas.Handle, 0,0, SRCCOPY);
  finally
    bmpXOR.Free;
    bmpAND.Free;
    bmpINVAND.Free;
    bmpTarget.Free;
  end;
end;


initialization

finalization

end.

