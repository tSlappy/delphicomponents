unit TestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  PropertiesForm;

type
  TFormTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest: TFormTest;
  PropForm: TPropForm;

implementation

{$R *.dfm}

// Initialize ProjectProperties and show the dialog
// There are 2 files with predefined options: NSIS.nsisproj and InnoSetup.innoproj
// These files are taken from RAD & Installer (www.rad-installer.com)

procedure TFormTest.Button1Click(Sender: TObject);
begin
  PropForm := TPropForm.Create(Self);
  PropForm.InitializeProject(True, 'InnoSetup.innoproj', 'Debug', 'Win32');
  PropForm.ShowModal;
end;

procedure TFormTest.Button2Click(Sender: TObject);
begin
  PropForm := TPropForm.Create(Self);
  PropForm.InitializeProject(False, 'NSIS.nsisproj', 'Release', 'Win32');
  PropForm.ShowModal;
end;

end.
