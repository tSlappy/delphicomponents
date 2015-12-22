program TestApp;

uses
  Forms,
  SysUtils,
  PropertiesForm in 'PropertiesForm.pas' {PropForm},
  UtilityFunctions in 'UtilityFunctions.pas',
  TestForm in 'TestForm.pas' {FormTest};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
