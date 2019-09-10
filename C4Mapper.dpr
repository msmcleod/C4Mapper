program C4Mapper;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  uMain in 'uMain.pas' {Form1},
  uCWPlugin in 'uCWPlugin.pas',
  uMackiePlugin in 'uMackiePlugin.pas',
  XMLClass in 'XMLClass.pas',
  xmldef in 'XMLDef.pas',
  FolderHelper in 'FolderHelper.pas';

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
