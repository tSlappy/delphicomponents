unit TestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Generics.Collections,

  NavigationBarsDockForm, DropDownMember;

type
  TFormTest = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest: TFormTest;
  NavigationBarsDockForm: TNavigationBarsDockableForm;

implementation

{$R *.dfm}

procedure TFormTest.Button1Click(Sender: TObject);
var
  types, members: TList<TDropDownMember>;
begin
  NavigationBarsDockForm := TNavigationBarsDockableForm.Create(Self);
  NavigationBarsDockForm.ShowDockForm;

  // Create sample data
  types := TList<TDropDownMember>.Create;
  members := TList<TDropDownMember>.Create;

  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'property', 6));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'not used', 7));
  types.Add(TDropDownMember.Create([FONTATTR_ITALIC], 'symbol', 8));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'file', 9));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'section', 10));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'group', 11));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'value', 12));
  types.Add(TDropDownMember.Create([FONTATTR_BOLD], 'class', 13));
  types.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'link', 14));

  members.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'variable', 0));
  members.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'static var', 1));
  members.Add(TDropDownMember.Create([FONTATTR_ITALIC], 'global', 2));
  members.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'function', 3));
  members.Add(TDropDownMember.Create([FONTATTR_BOLD], 'procedure', 4));
  members.Add(TDropDownMember.Create([FONTATTR_PLAIN], 'event function', 5));

  NavigationBarsDockForm.SynchronizeDropdowns(types, members);
end;

end.
