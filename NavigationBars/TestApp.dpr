program TestApp;

uses
  Forms,
  SysUtils,
  TestForm in 'TestForm.pas' {FormTest},
  AutoCompleteEdit in 'AutoCompleteEdit.pas',
  BaseDockForm in 'BaseDockForm.pas' {BaseDockableForm},
  NavigationBarsDockForm in 'NavigationBarsDockForm.pas' {NavigationBarsDockableForm},
  DropDownMember in 'DropDownMember.pas',
  UtilityFunctions in 'UtilityFunctions.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
