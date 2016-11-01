unit PropertiesForm;

interface

// Use this symbol to build the component with RAD Studio OpenToolsAPI
{ $DEFINE TOOLSAPI_BUILD}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PropertiesPage, ComCtrls, ExtCtrls, MSXML, ComObj;

type
  TPropForm = class(TForm)
    LabelTarget: TLabel;
    ComboBoxTarget: TComboBox;
    LabelConfiguration: TLabel;
    ComboBoxConfiguration: TComboBox;
    ShapeDetails: TShape;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    LabelDetails: TLabel;
    LabelDescription: TLabel;
    ProjectProperties: TPropertiesPage;
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxConfigurationChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);

  private
    { Private-Deklarationen }
    xmlDoc: IXMLDOMDocument2;
    isInnoSetup: Boolean;
    projPath: String;
    projConfiguration: String;
    currentConfigurationNode: IXMLDOMNode;

    function PutSymbol(Symbol, Text: String; Sep: Char): String;
    function StripSymbol(Symbol, Text: String): String;

  public
    { Public-Deklarationen }
    procedure InitializeProject(innoSetup: Boolean; Path, Configuration: String);
    function LoadProjectXml: Boolean;
    function SaveProjectXml: Boolean;
    procedure InitInnoprojProperties;
    procedure InitNsisprojProperties;
    function GetXmlStringValue(Element, Default: String): String;

    function OnItemDetails(Name, Desc: String): Integer; stdcall;
  end;

var
  PropForm: TPropForm;

implementation

uses
  UtilityFunctions {$IFDEF TOOLSAPI_BUILD}, ToolsAPI {$ENDIF}, ShellAPI;

{$R *.DFM}

procedure TPropForm.ButtonCancelClick(Sender: TObject);
begin
  // Ignore all changes
  //ProjectProperties.Modified := False; 
  Close;
end;

procedure TPropForm.ButtonHelpClick(Sender: TObject);
begin
  // Show help
  ShellExecute(0, 'OPEN', PChar('http://www.rad-installer.com/features.html#project-properties'), '', '', SW_SHOWNORMAL);
end;

procedure TPropForm.ButtonOKClick(Sender: TObject);
var
{$IFDEF TOOLSAPI_BUILD}
  ActiveProject: IOTAProject;
  Configurations: IOTAProjectOptionsConfigurations;
{$ENDIF}
  I: Integer;
  node: IXMLDOMNode;

begin
  // Save configuration change

{$IFDEF TOOLSAPI_BUILD}
  ActiveProject := GetActiveProject;
  Configurations := ActiveProject.ProjectOptions as IOTAProjectOptionsConfigurations;
  for I := 0 to Configurations.ConfigurationCount - 1 do
  begin
    if(Configurations.Configurations[I].Name = projConfiguration) then
    begin
      Configurations.ActiveConfiguration := Configurations.Configurations[I];
      // Write configuration to .innoproj/.nsisproj file

      // Find appropriate element and get it's value                                  vvvvv
      { <Project><PropertyGroup><Configuration Condition=" '$(Configuration)' == '' ">Debug</Configuration> }
      node := xmlDoc.selectSingleNode('/Project/PropertyGroup/Configuration');
      if ((node.attributes.length > 0) and (StringUtils_Contains_IC(String(node.attributes.getNamedItem('Condition').Text), '$(Configuration)'))) then
      begin
        // Change current configuration
        node.text := projConfiguration;
        break;
      end;
      break;
    end;
  end;
{$ENDIF}

  //if(ProjectProperties.Modified) then
    SaveProjectXml;
  ProjectProperties.Modified := False;
  Close;
end;

procedure TPropForm.ComboBoxConfigurationChange(Sender: TObject);
begin
  projConfiguration := ComboBoxConfiguration.Items[ComboBoxConfiguration.ItemIndex];

  if(ProjectProperties.Modified) then
    if (MessageDlg('Project Properties were changed. Do you want to save changes?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
      SaveProjectXml
    else
      ProjectProperties.Modified := False;

  // Load Project (.innoproj or .nsisproj)
  if (not LoadProjectXml) then
    MessageDlg('Cannot load Project Properties from file! Default values will be used!', mtWarning, [mbOK], 0);

  if(isInnoSetup) then
  begin
    InitInnoprojProperties;
    Caption := 'Inno Setup Project Properties';
  end
  else
  begin
    InitNsisprojProperties;
    Caption := 'NSIS Project Properties';
  end;

  LabelDetails.Caption := '';
  LabelDescription.Caption := '';
end;

procedure TPropForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if(ProjectProperties.Modified) then
    case (MessageDlg('Project Properties were changed. Do you want to save changes?', mtWarning, [mbYes, mbNo, mbCancel], 0)) of
      mrYes:
      begin
        SaveProjectXml;
        CanClose := True;
      end;
      mrNo:
      begin
        CanClose := True;
      end;
      mrCancel:
      begin
        CanClose := False;
      end;
    end
  else
    CanClose := True;
end;

procedure TPropForm.FormDestroy(Sender: TObject);
begin
  ProjectProperties.FreeMemory;
end;

procedure TPropForm.InitInnoprojProperties;
var
  Category, Item: TPropEditData;

begin
  // Create categories and items
  ProjectProperties.ClearItems;

  Category.Kind := peCategory;
  Category.Name := 'Compiler';
  ProjectProperties.CreateItem(Category);

  Item.Kind := peItem;
  Item.Name := 'Output (/O)';
  Item.Description := 'Output files to specified path (overrides OutputDir): /O<path>.';
  Item.XmlTag := 'CompilerOutput';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/O';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'OutputBaseFilename (/F)';
  Item.Description := 'Overrides OutputBaseFilename with the specified filename: /F<filename>.';
  Item.XmlTag := 'CompilerBaseFileName';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/F';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'SignTool (/S)';
  Item.Description := 'Sets a SignTool with the specified name and command: /S<name>=<command>.';
  Item.XmlTag := 'CompilerSignTool';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/S';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  //////////////////////////////////////////////////////////////////////////////
  Category.Kind := peCategory;
  Category.Name := 'Directives';
  ProjectProperties.CreateItem(Category);

  Item.Kind := peItem;
  Item.Name := 'Symbol (/D)';
  Item.Description := 'Emulate #define public <name> <value>  /D<name>[=<value>].';
  Item.XmlTag := 'DirectivesSymbol';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/D';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Option (/$)';
  Item.Description := 'Emulate #pragma option -<letter>(+|-)  /$<letter>(+|-).';
  Item.XmlTag := 'DirectivesOption';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/$';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Parse Option (/P)';
  Item.Description := 'Emulate #pragma parseroption -<letter>(+|-)  /P<letter>(+|-).';
  Item.XmlTag := 'DirectivesParseOption';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/P';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Include (/I)';
  Item.Description := 'Emulate #pragma include <paths>  /I<paths>.';
  Item.XmlTag := 'DirectivesInclude';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/I';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Include File (/J)';
  Item.Description := 'Emulate #include <filename>  /J<filename>.';
  Item.XmlTag := 'DirectivesIncludeFile';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/J';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Inline Start (/{#)';
  Item.Description := 'Emulate #pragma inlinestart <string>  /{#<string>.';
  Item.XmlTag := 'DirectivesInlineStart';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/{#';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Inline End (/})';
  Item.Description := 'Emulate #pragma inlineend <string>  /}<string>.';
  Item.XmlTag := 'DirectivesInlineEnd';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/}';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  //////////////////////////////////////////////////////////////////////////////
  Category.Kind := peCategory;
  Category.Name := 'General';
  ProjectProperties.CreateItem(Category);
  
  Item.Kind := peItem;
  Item.Name := 'Run installer';
  Item.Description := 'Run resulting installer after successful compilation.';
  Item.XmlTag := 'RunInstaller';
  Item.Default := 'Yes';
  Item.Possible := 'Yes|No';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Launch tool before build';
  Item.Description := 'Launch specified application before build. Output of this application is redirected into Messages window (Output tab - useful for batch files or command line utilities).';
  Item.XmlTag := 'LaunchToolBeforeBuild';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '';
  Item.Typ := piString;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Launch tool after build';
  Item.Description := 'Launch specified application after successful build. Output of this application is redirected into Messages window (Output tab - useful for batch files or command line utilities).';
  Item.XmlTag := 'LaunchToolAfterBuild';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '';
  Item.Typ := piString;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);
end;

procedure TPropForm.InitNsisprojProperties;
var
  Category, Item: TPropEditData;

begin
  // Create categories and items
  ProjectProperties.ClearItems;

  Category.Kind := peCategory;
  Category.Name := 'Compiler';
  ProjectProperties.CreateItem(Category);

  Item.Kind := peItem;
  Item.Name := '/NOCD';
  Item.Description := '/NOCD disables the current directory change to that of the .nsi file.';
  Item.XmlTag := 'CompilerNoCD';
  Item.Default := '';
  Item.Possible := '|/NOCD';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := '/NOCONFIG';
  Item.Description := '/NOCONFIG disables inclusion of <path to makensis.exe>/nsisconf.nsh.';
  Item.XmlTag := 'CompilerNoConfig';
  Item.Default := '';
  Item.Possible := '|/NOCONFIG';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Process Priority (/Px)';
  Item.Description := '/Px sets the compiler process priority, where x is 5=real time, 4=high, 3=above normal, 2=normal, 1=below normal, 0=idle.';
  Item.XmlTag := 'CompilerPriority';
  Item.Default := '/P2';
  Item.Possible := '/P0|/P1|/P2|/P3|/P4|/P5';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := '/RAW';
  Item.Description := 'If /RAW is set the MakeNSIS.exes output is binary (allowing Unicode characters), otherwise text. This option is supported only in Unicode NSIS (2.46-5) and it is not present in NSIS 3.';
  Item.XmlTag := 'CompilerRaw';
  Item.Default := '';
  Item.Possible := '|/RAW';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);
  
  Item.Kind := peItem;
  Item.Name := '/WX';
  Item.Description := 'Treats warnings as errors.';
  Item.XmlTag := 'CompilerWarningsX';
  Item.Default := '';
  Item.Possible := '|/WX';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := '/PPO';
  Item.Description := 'Runs only the preprocessor (Prints the result to Output Window).';
  Item.XmlTag := 'CompilerPPO';
  Item.Default := '';
  Item.Possible := '|/PPO';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := '/SAFEPPO';
  Item.Description := 'Runs only the preprocessor (Prints the result to Output Window). The safe version will not execute instructions like !appendfile or !system. !packhdr and !finalize are never executed.';
  Item.XmlTag := 'CompilerSafePPO';
  Item.Default := '';
  Item.Possible := '|/SAFEPPO';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);  

  //////////////////////////////////////////////////////////////////////////////
  Category.Kind := peCategory;
  Category.Name := 'User Symbols';
  ProjectProperties.CreateItem(Category);

  Item.Kind := peItem;
  Item.Name := 'Define (/D)';
  Item.Description := '/Ddefine[=value] defines the symbol "define" for the script [to value]. Parameters are processed by order! (/Ddef ins.nsi != ins.nsi /Ddef)';
  Item.XmlTag := 'UserSymbolsDefine';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/D';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Execute (/X)';
  Item.Description := '/Xscriptcmd executes scriptcmd in script (i.e. "/XOutFile poop.exe").';
  Item.XmlTag := 'UserSymbolsExecute';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '/X';
  Item.Typ := piString;
  Item.Value := StripSymbol(Item.Strip, GetXmlStringValue(Item.XmlTag, Item.Default));
  Item.AddSpace := True;
  ProjectProperties.CreateItem(Item);

  //////////////////////////////////////////////////////////////////////////////
  Category.Kind := peCategory;
  Category.Name := 'General';
  ProjectProperties.CreateItem(Category);
  
  Item.Kind := peItem;
  Item.Name := 'Run installer';
  Item.Description := 'Run resulting installer after successful compilation.';
  Item.XmlTag := 'RunInstaller';
  Item.Default := 'Yes';
  Item.Possible := 'Yes|No';
  Item.Strip := '';
  Item.Typ := piCombo;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);  

  Item.Kind := peItem;
  Item.Name := 'Launch tool before build';
  Item.Description := 'Launch specified application before build. Output of this application is redirected into Messages window (Output tab - useful for batch files or command line utilities).';
  Item.XmlTag := 'LaunchToolBeforeBuild';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '';
  Item.Typ := piString;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);

  Item.Kind := peItem;
  Item.Name := 'Launch tool after build';
  Item.Description := 'Launch specified application after successful build. Output of this application is redirected into Messages window (Output tab - useful for batch files or command line utilities).';
  Item.XmlTag := 'LaunchToolAfterBuild';
  Item.Default := '';
  Item.Possible := '';
  Item.Strip := '';
  Item.Typ := piString;
  Item.Value := GetXmlStringValue(Item.XmlTag, Item.Default);
  Item.AddSpace := False;
  ProjectProperties.CreateItem(Item);
end;

function TPropForm.LoadProjectXml: Boolean;
var
  node: IXMLDomNode;
  I: Integer;
  Temp: String;
  propertyGroup: IXMLDomNodeList;

begin
  try
    // Load the Project (.innoproj or .nsisproj file)
    xmlDoc := nil;
    currentConfigurationNode := nil;
    xmlDoc := CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument2;
    xmlDoc.async := False;
    xmlDoc.load(projPath);
    if (xmlDoc.parseError.errorCode <> 0) then
    begin
      xmlDoc := nil;
      currentConfigurationNode := nil;
      raise Exception.Create('Cannot not load Project file! Details: ' + xmlDoc.parseError.reason);
    end;

    // Find appropriate element and get it's value
    { <?xml><Project><PropertyGroup Condition=" '$(Configuration)' == 'XXX' "> }
    propertyGroup := xmlDoc.selectNodes('/Project/PropertyGroup');
    for I := 0 to propertyGroup.length - 1 do
    begin
      node := propertyGroup[I];
      if (node.attributes.length > 0) then
      begin
        Temp := String(node.attributes.getNamedItem('Condition').Text);
        if(StringUtils_Contains_IC(Temp, '$(Configuration)') and StringUtils_Contains_IC(Temp, projConfiguration)) then
        begin
          // Do all operations on this node
          currentConfigurationNode := propertyGroup[I];
          break;
        end;
      end;
    end;

    Result := True;
  except
    on Exception do
      Result := False;
  end;
end;

function TPropForm.SaveProjectXml: Boolean;
var
  I: Integer;
  PPED: PPropEditData;
  node: IXMLDOMNode;
  elementNode: IXMLDOMNode;
  Ch: Char;

begin
  try
    // Save the Project (.innoproj or .nsisproj file)
    if((xmlDoc <> nil) and (currentConfigurationNode <> nil)) then
    begin
      for I := 0 to ProjectProperties.Items.Count - 1 do
      begin
          PPED := Pointer(ProjectProperties.Items.Objects[I]);
          if(PPED^.Kind = peCategory) then
            continue;

          // This is correct Node for selected Configuration
          node := currentConfigurationNode.selectSingleNode(PPED^.XmlTag);

          if(PPED^.Value = '') then
          begin
            // Delete element if exists (no empty values are allowed)
            if(node <> nil) then
              currentConfigurationNode.removeChild(node);
          end
          else
          begin
            if(PPED^.AddSpace) then
              Ch := #32
            else
              Ch := #0;
            if(node <> nil) then
              node.text := PutSymbol(PPED^.Strip, PPED^.Value, Ch)
            else
            begin
              // Do the save as above + Create new element first
              elementNode := currentConfigurationNode.appendChild(xmlDoc.createNode(NODE_ELEMENT, PPED^.XmlTag, 'http://schemas.microsoft.com/developer/msbuild/2003'));
              elementNode.text := PutSymbol(PPED^.Strip, PPED^.Value, Ch);
            end;
          end;
      end;

      xmlDoc.save(projPath);
      Result := True;
    end;
  except
    on Exception do
      Result := False;
  end;
end;

function TPropForm.OnItemDetails(Name, Desc: String): Integer; stdcall;
begin
  LabelDetails.Caption := Name;
  LabelDescription.Caption := Desc;
end;

procedure TPropForm.InitializeProject(innoSetup: Boolean; Path, Configuration: String);
begin
  isInnoSetup := innoSetup;
  projPath := Path;

  if((Configuration <> 'Debug') and (Configuration <> 'Release')) then
    Configuration := 'Debug';
  projConfiguration := Configuration;

  // Load Project (.innoproj or .nsisproj)
  if (not LoadProjectXml) then
    MessageDlg('Cannot load Project Properties from file! Default values will be used!', mtWarning, [mbOK], 0);

  if(isInnoSetup) then
  begin
    InitInnoprojProperties;
    Caption := 'Inno Setup Project Properties';
  end
  else
  begin
    InitNsisprojProperties;
    Caption := 'NSIS Project Properties';
  end;

  if(projConfiguration = 'Release') then
    ComboBoxConfiguration.ItemIndex := 1
  else
    ComboBoxConfiguration.ItemIndex := 0;

  ProjectProperties.OnDetailsCallback := OnItemDetails;
end;

function TPropForm.StripSymbol(Symbol, Text: String): String;
begin
  // Remove Symbol from Text and do Trim()
  Result := Trim(StringReplace(Text, Symbol, '', [rfReplaceAll, rfIgnoreCase]));
end;

function TPropForm.PutSymbol(Symbol, Text: String; Sep: Char): String;
begin
  // Add Symbol to Text and add space to the end (optional)
  Result := Symbol + Text + Sep;
end;

function TPropForm.GetXmlStringValue(Element, Default: String): String;
var
  node: IXMLDOMNode;
begin
  Result := Default;
  if((xmlDoc = nil) or (currentConfigurationNode = nil)) then
    Exit;

  // This os correct Node for selected Configuration
  node := currentConfigurationNode.selectSingleNode(Element);
  if(node <> nil) then
  begin
    Result := node.text;
  end;
end;

end.

