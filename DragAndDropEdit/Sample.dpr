program Sample;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  DropFilesTarget in 'DropFilesTarget.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
