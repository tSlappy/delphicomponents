unit BaseDockForm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms;

type
  TBaseDockableForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBaseDockableFormClass = class of TBaseDockableForm;

procedure ShowDockableForm(Form: TBaseDockableForm);
procedure HideDockableForm(Form: TBaseDockableForm);
procedure CreateDockableForm(var FormVar: TBaseDockableForm; FormClass: TBaseDockableFormClass);
procedure FreeDockableForm(var FormVar: TBaseDockableForm);

implementation

{$R *.dfm}


procedure ShowDockableForm(Form: TBaseDockableForm);
begin
  if not Assigned(Form) then
    Exit;
  if not Form.Floating then
  begin
    try
      Form.ShowModal;

    except
      // Exception
    end;
  end
  else
    try
      Form.Show;
    except
      // Exception
    end;
end;

procedure HideDockableForm(Form: TBaseDockableForm);
begin
  if not Assigned(Form) then
    Exit;

  try
    Form.Hide;
  except
    // Exception
  end;
end;

procedure RegisterDockableForm(FormClass: TBaseDockableFormClass;
  var FormVar; const FormName: string);
begin

end;

procedure UnRegisterDockableForm(var FormVar; const FormName: string);
begin

end;

procedure CreateDockableForm(var FormVar: TBaseDockableForm; FormClass: TBaseDockableFormClass);
begin
  TCustomForm(FormVar) := FormClass.Create(nil);
  RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);
end;

procedure FreeDockableForm(var FormVar: TBaseDockableForm);
begin
  if Assigned(FormVar) then
  begin
    UnRegisterDockableForm(FormVar, FormVar.Name);
    FreeAndNil(FormVar);
  end;
end;

{ TIDEDockableForm }

constructor TBaseDockableForm.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TBaseDockableForm.Destroy;
begin

  inherited;
end;

end.
