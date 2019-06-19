unit FolderHelper;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TFolderHelper = class
  public
    class function GetMyDocumentsFolder: string;
  end;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils;

type
  KNOWNFOLDERID = TGuid;

  function SHGetKnownFolderPath(const rfid: KNOWNFOLDERID; dwFlags: DWORD; hToken: THandle; out ppszPath: PWideChar): HResult; stdcall; external 'Shell32.dll';

procedure CoTaskMemFree(pv: Pointer); stdcall; external 'ole32.dll';

class function TFolderHelper.GetMyDocumentsFolder: string;
var
  pf : TGuid;
  PFolderPath : PWideChar;
begin
  result := '';
  pf := StringToGUID('{FDD39AD0-238F-46AF-ADB4-6C85480369C7}');

  if (SHGetKnownFolderPath(pf, 0, 0, PFolderPath) = S_OK) then
  begin
    Result := PFolderPath;
    CoTaskMemFree(PFolderPath);
  end;
end;

end.
