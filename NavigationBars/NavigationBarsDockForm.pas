unit NavigationBarsDockForm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  DropDownMember,
  Generics.Collections,
  BaseDockForm,
  AutoCompleteEdit, ImgList, Buttons, System.ImageList;

const
  coControlsSpace   = 2;    // Pixels
  coControlsLeftTop = 8;    // Default Left and Top position
  coControlsHeight  = 22;   // Default Height for Buttons (Square!)
  coFormMinWidth    = 200;  // Minimum Form width (ClientWidth)
  coFormMinHeight   = 40;   // Minimum Form height (ClientHeight)

type
 // PListDropDownMembers = ^TList<TDropDownMember>;

  TNavigationBarsDockableForm = class(TBaseDockableForm)
    SpeedButtonTypes: TSpeedButton;
    SpeedButtonMembers: TSpeedButton;
    ImageListIcons: TImageList;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButtonTypesClick(Sender: TObject);
    procedure SpeedButtonMembersClick(Sender: TObject);
  public
    class procedure RemoveDockForm;
    class procedure ShowDockForm;
    class procedure HideDockForm;
    class procedure CreateDockForm;

    class procedure SynchronizeFile(fileName: String);
    class procedure SynchronizeDropdowns(dropDownTypes, dropDownMembers: TList<TDropDownMember>);

    class procedure ShowTypes;
    class procedure ShowMembers;

  private
    AutoCompleteEditTypes: TAutoCompleteEdit;
    AutoCompleteEditMembers: TAutoCompleteEdit;

    mDropDownTypes, mDropDownMembers: TList<TDropDownMember>;
    mCurrentFile: String;
    mShowNavBars: Boolean;

    procedure CreateControls;
    procedure OnListHideTypes(Sender: TObject);
    procedure OnListHideMembers(Sender: TObject);

  end;

implementation

{$R *.dfm}

uses
  Dialogs;

var
  FormInstance: TBaseDockableForm = nil;

{ TNavigationBarsDockableForm }

class procedure TNavigationBarsDockableForm.CreateDockForm;
begin
  if not Assigned(FormInstance) then
    CreateDockableForm(FormInstance, TNavigationBarsDockableForm);
end;

class procedure TNavigationBarsDockableForm.ShowDockForm;
begin
  CreateDockForm;
  ShowDockableForm(FormInstance);
end;

class procedure TNavigationBarsDockableForm.HideDockForm;
begin
  HideDockableForm(FormInstance);
end;

procedure TNavigationBarsDockableForm.SpeedButtonMembersClick(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  ImageListIcons.GetBitmap(1, Bitmap);
  SpeedButtonMembers.Glyph := Bitmap;
  Bitmap.Free;

  AutoCompleteEditMembers.ShowAllItems;
end;

procedure TNavigationBarsDockableForm.SpeedButtonTypesClick(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  ImageListIcons.GetBitmap(1, Bitmap);
  SpeedButtonTypes.Glyph := Bitmap;
  Bitmap.Free;

  AutoCompleteEditTypes.ShowAllItems;
end;

procedure TNavigationBarsDockableForm.OnListHideTypes(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  ImageListIcons.GetBitmap(0, Bitmap);
  SpeedButtonTypes.Glyph := Bitmap;
  Bitmap.Free;
end;

procedure TNavigationBarsDockableForm.OnListHideMembers(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  ImageListIcons.GetBitmap(0, Bitmap);
  SpeedButtonMembers.Glyph := Bitmap;
  Bitmap.Free;
end;

procedure TNavigationBarsDockableForm.FormCreate(Sender: TObject);
begin
  inherited;

  mDropDownTypes := nil;
  mDropDownMembers := nil;

  // Init form
  ImageListIcons.GetBitmap(0, SpeedButtonTypes.Glyph);
  ImageListIcons.GetBitmap(0, SpeedButtonMembers.Glyph);
end;

procedure TNavigationBarsDockableForm.CreateControls;
const
  ECM_FIRST = $1500;
  EM_SETCUEBANNER = ECM_FIRST + 1;

begin
  // Create controls
  AutocompleteEditTypes := TAutoCompleteEdit.Create(Self, 'Types: Alt + T');
  AutocompleteEditTypes.Parent := Self;
  AutocompleteEditTypes.Visible := True;
  AutocompleteEditTypes.Left := coControlsLeftTop;
  AutocompleteEditTypes.Top := coControlsLeftTop;
  SendMessage(AutocompleteEditTypes.Handle, EM_SETCUEBANNER, 0, LParam(PWideChar(WideString('Types: Alt + T'))));
  AutocompleteEditTypes.OnListHide := OnListHideTypes;

  AutocompleteEditMembers := TAutoCompleteEdit.Create(Self, 'Members: Alt + M');
  AutocompleteEditMembers.Parent := Self;
  AutocompleteEditMembers.Visible := True;
  AutocompleteEditMembers.Left := 100;
  AutocompleteEditMembers.Top := coControlsLeftTop;
  SendMessage(AutocompleteEditMembers.Handle, EM_SETCUEBANNER, 0, LParam(PWideChar(WideString('Members: Alt + M'))));
  AutocompleteEditMembers.OnListHide := OnListHideMembers;

  mShowNavBars := True;

end;

class procedure TNavigationBarsDockableForm.SynchronizeDropdowns(dropDownTypes, dropDownMembers: TList<TDropDownMember>);
var
  I: Integer;
begin
  if Assigned(FormInstance) then
  begin
    TNavigationBarsDockableForm(FormInstance).AutoCompleteEditTypes.ClearData;
    for i := 0 to dropDownTypes.Count - 1 do
    begin
      TNavigationBarsDockableForm(FormInstance).AutoCompleteEditTypes.AddMember(dropDownTypes[i]);
    end;

    TNavigationBarsDockableForm(FormInstance).AutoCompleteEditMembers.ClearData;
    for i := 0 to dropDownMembers.Count - 1 do
    begin
      TNavigationBarsDockableForm(FormInstance).AutoCompleteEditMembers.AddMember(dropDownMembers[i]);
    end;


    FormInstance.Caption := 'Navigation Bars' + ExtractFileName((FormInstance as TNavigationBarsDockableForm).mCurrentFile);
  end;
end;

class procedure TNavigationBarsDockableForm.SynchronizeFile(fileName: String);
var
  CorrectFile: Boolean;

begin
  CorrectFile := False;

  TNavigationBarsDockableForm.CreateDockForm;
  (FormInstance as TNavigationBarsDockableForm).mCurrentFile := fileName;

  //if(String_Contains(fileName, coINNO_ISS)) then
  begin
    if(FormInstance.Floating) then
      FormInstance.Caption := 'Navigation Bars' + ExtractFileName(fileName) + ' [Parsing...]'
    else
      FormInstance.Caption := 'Navigation Bars ' + ExtractFileName(fileName);

    CorrectFile := true;
  end;

  if(CorrectFile And TNavigationBarsDockableForm(FormInstance).mShowNavBars) then
    ShowDockForm
  else
    HideDockForm;
end;

procedure TNavigationBarsDockableForm.FormResize(Sender: TObject);
var
  Temp: Integer;
  Pom: Double;

begin
  inherited;
  if((AutoCompleteEditTypes = nil) or (AutoCompleteEditMembers = nil)) then
    CreateControls
  else
    ShowDockForm;

  // Center controls: Types = 40%, Members = 60 % of Width. Height is fixed (default)
  // _ _ _ _ _ _ _ X | _ _ _ _ _ _ _ _ _ _ _ _ _ _ X
  // There is 2 * 2 px space between components
  // Left is fixed to 8 px

  // Width := (8 + Types.Width + Button + 2 + 2 + Members.Width + Button + 8)
  Temp := (Sender as TBaseDockableForm).ClientWidth - (coControlsSpace * 2) - (coControlsLeftTop * 2) - (coControlsHeight * 2);

  AutoCompleteEditTypes.Left := coControlsLeftTop;
  AutoCompleteEditTypes.Top := coControlsLeftTop;
  AutoCompleteEditTypes.Width := Round(Temp * 0.4);
  AutoCompleteEditTypes.Height := coControlsHeight;

  SpeedButtonTypes.Left := AutoCompleteEditTypes.Left + AutoCompleteEditTypes.Width;
  SpeedButtonTypes.Top := coControlsLeftTop;
  SpeedButtonTypes.Width := coControlsHeight;
  SpeedButtonTypes.Height := coControlsHeight;

  AutoCompleteEditMembers.Left := SpeedButtonTypes.Left + coControlsHeight + (coControlsSpace * 2);
  AutoCompleteEditMembers.Top := coControlsLeftTop;
  AutoCompleteEditMembers.Width := Round(Temp * 0.6);
  AutoCompleteEditMembers.Height := coControlsHeight;

  SpeedButtonMembers.Left := AutoCompleteEditMembers.Left + AutoCompleteEditMembers.Width;
  SpeedButtonMembers.Top := coControlsLeftTop;
  SpeedButtonMembers.Width := coControlsHeight;
  SpeedButtonMembers.Height := coControlsHeight;

  if((Sender as TBaseDockableForm).ClientHeight < coFormMinHeight) then
    (Sender as TBaseDockableForm).ClientHeight := coFormMinHeight;
end;

class procedure TNavigationBarsDockableForm.RemoveDockForm;
begin
  if(Assigned(FormInstance)) then
  begin
    try
      FreeAndNil((FormInstance as TNavigationBarsDockableForm).AutoCompleteEditTypes);
      FreeAndNil((FormInstance as TNavigationBarsDockableForm).AutoCompleteEditMembers);
      (FormInstance as TNavigationBarsDockableForm).mDropDownTypes.Clear;
      (FormInstance as TNavigationBarsDockableForm).mDropDownMembers.Clear;
      FreeAndNil((FormInstance as TNavigationBarsDockableForm).mDropDownTypes);
      FreeAndNil((FormInstance as TNavigationBarsDockableForm).mDropDownMembers);
    except

    end;
  end;

  FreeDockableForm(FormInstance);
end;

class procedure TNavigationBarsDockableForm.ShowTypes;
begin
  (FormInstance as TNavigationBarsDockableForm).SpeedButtonTypesClick(nil);
end;

class procedure TNavigationBarsDockableForm.ShowMembers;
begin
  (FormInstance as TNavigationBarsDockableForm).SpeedButtonMembersClick(nil);
end;

end.
