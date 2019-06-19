unit uCWPlugin;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, StdCtrls, ComCtrls;

type
  TCWPluginParameter = class(TObject)
  private
    FParamIndex: integer;
    FParamName: string;
    FControlType: string;
    procedure SetControlType(const Value: string);
    procedure SetParamIndex(const Value: integer);
    procedure SetParamName(const Value: string);
  public
    property ParamIndex : integer read FParamIndex write SetParamIndex;
    property ControlType : string read FControlType write SetControlType;
    property ParamName : string read FParamName write SetParamName;
  end;

  TCWPlugin = class(TObject)
  private
    FParameters : TObjectList;
    FClassID: string;
    FName: string;
    FIsSynth : boolean;
    FIsVST3 : boolean;
    FIsX64 : boolean;
    procedure SetClassID(const Value: string);
    procedure SetName(const Value: string);
    function GetParameter(Index: integer): TCWPluginParameter;
    procedure GetPluginTypeFromRegistry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddParameter(paramIndex: integer; controlType : string; paramName: string);
    procedure SortParameters;
    function GetParameterName(paramIndex : integer) : string;
    property ClassID : string read FClassID write SetClassID;
    property Name : string read FName write SetName;
    property Parameters : TObjectList read FParameters;
    property Parameter[Index : integer] : TCWPluginParameter read GetParameter;
    property IsSynth : boolean read FIsSynth;
    property IsVST3 : boolean read FIsVST3;
    property IsX64 : boolean read FIsX64;
  end;

  TCWPluginList = class(TObjectList)
  private
    FProgressBar : TProgressBar;
    procedure ParseProgress(Sender : TObject; CurrentIndex, FileSize : integer);
  public
    constructor Create; overload;
    constructor Create(OwnsObjects : boolean); overload;
    procedure LoadCWPlugins(StatusLabel : TLabel; Progress : TProgressBar);
  end;

  procedure CWPluginListReset;

var
  CWPluginList : TCWPluginList;
  RegistryPluginList : TStringList;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  XMLClass, XMLDef, Forms, Registry, Dialogs;

function CompareCWParameterIds(Item1, Item2: Pointer): Integer;
begin
  if (TCWPluginParameter(Item1).ParamIndex < TCWPluginParameter(Item2).ParamIndex) then
    result := -1
  else if (TCWPluginParameter(Item1).ParamIndex > TCWPluginParameter(Item2).ParamIndex) then
    result := 1
  else
    result := 0;
end;

constructor TCWPluginList.Create(OwnsObjects: boolean);
begin
  inherited Create(OwnsObjects);
  FProgressBar := nil;
end;

constructor TCWPluginList.Create;
begin
  inherited Create(true);
  FProgressBar := nil;
end;

procedure TCWPluginList.LoadCWPlugins(StatusLabel: TLabel; Progress: TProgressBar);
var
  xmlParser : TXMLParser;
  pluginCount : integer;
  plugins, plugin, param, pluginsCont : TXMLElement;
  xmlObject : TXMLObject;
  i,j : integer;
  cwplugin : TCWPlugin;
  s : string;
  ctrlType, ctrlName, ctrlIndex : string;
  idx : integer;
  fs : TStream;
  genericpluginparams : string;
begin
  Self.Clear;
  FProgressBar := Progress;

  xmlParser := TXMLParser.Create(nil);
  xmlParser.OnParseProgress := Self.ParseProgress;

  try
    try
      if (StatusLabel <> nil) then
      begin
        StatusLabel.Caption := 'Reading Cakewalk genericpluginparams.xml';
        StatusLabel.Refresh;
        Application.ProcessMessages;
      end;

      genericpluginparams := SysUtils.GetEnvironmentVariable('APPDATA') + '\Cakewalk\ACT Data\genericpluginparams.xml';
      fs := TFileStream.Create(genericpluginparams, fmShareDenyNone + fmOpenRead);
      xmlParser.ParseFromStream(fs);

      if (xmlParser.XMLDocument.ChildCount = 1) then // plugin_params
      begin
        xmlObject := xmlParser.XMLDocument.ChildItem(0);
        if (xmlObject is TXMLElement) and (TXMLElement(xmlObject).ChildCount > 1) then
        begin
          pluginsCont := TXMLElement(xmlObject);
          plugins := nil;
          i := 0;
          while (i < pluginsCont.ChildCount) do
          begin
            xmlObject := pluginsCont.ChildItem(i);

            if ((xmlObject is TXMLElement) and (TXMLElement(xmlObject).FTag = 'plugins')) then
            begin
              plugins := TXMLElement(xmlObject);
              break;
            end;

            inc(i);
          end;

          if (plugins <> nil) then
          begin
            pluginCount := plugins.ChildCount;

            for i := 0 to plugins.ChildCount-1 do
            begin
              if (Progress <> nil) then
              begin
                Progress.Position := round((100 / pluginCount) * i);
              end;

              if (StatusLabel <> nil) then
              begin
                StatusLabel.Caption := 'Processing Plugin ' + inttostr(i) + ' / ' + inttostr(pluginCount);
                StatusLabel.Refresh;
                Application.ProcessMessages;
              end;

              plugin := TXMLElement(plugins.ChildItem(i));
              cwplugin := TCWPlugin.Create;

              if (plugin.GetAttribute('name', s) = XML_OK) and (plugin.ChildCount > 0) then
              begin
                cwplugin.Name := trim(s);
                cwplugin.ClassID := plugin.FTag;

                for j := 0 to plugin.ChildCount-1 do
                begin
                  param := TXMLElement(plugin.ChildItem(j));
                  if (param.GetAttribute('paramindex', ctrlIndex) = XML_OK) and
                     (param.GetAttribute('controltype', ctrlType) = XML_OK) and
                     (param.GetAttribute('paramname', ctrlName) = XML_OK) then
                  begin
                    try
                      idx := strtoint(ctrlIndex);
                      cwplugin.AddParameter(idx, ctrlType, ctrlName);
                    except
                      FreeAndNil(cwplugin);
                      break;
                    end;
                  end;
                end;

                cwplugin.SortParameters;

                if (cwplugin <> nil) then
                  Self.Add(cwplugin);
              end
              else
              begin
                FreeAndNil(cwplugin);
              end;
            end;
          end;
        end;
      end;

    except
      on e:Exception do ;  // TODO: handle this rather than just stop processing?
    end;
  finally
    FreeAndNil(fs);
    FreeAndNil(xmlParser);
    if (Progress <> nil) then
      Progress.Position := 0;
  end;
end;

{ TCWPluginParameter }

procedure TCWPluginParameter.SetControlType(const Value: string);
begin
  FControlType := Value;
end;

procedure TCWPluginParameter.SetParamIndex(const Value: integer);
begin
  FParamIndex := Value;
end;

procedure TCWPluginParameter.SetParamName(const Value: string);
begin
  FParamName := Value;
end;

{ TCWPlugin }

procedure TCWPlugin.AddParameter(paramIndex: integer; controlType : string; paramName: string);
var
  param : TCWPluginParameter;
begin
  param := TCWPluginParameter.Create;
  param.ParamIndex := paramIndex;
  param.ControlType := controlType;
  param.ParamName := paramName;
  FParameters.Add(param);
end;

constructor TCWPlugin.Create;
begin
  FParameters := TObjectList.Create(true);
  FIsSynth := false;
  FIsVST3 := false;
  FIsX64 := true;
  FClassID := '';
  FName:= '';
end;

destructor TCWPlugin.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

function TCWPlugin.GetParameter(Index: integer): TCWPluginParameter;
begin
  if Index < FParameters.Count then
    result := TCWPluginParameter(FParameters[Index])
  else
    result := nil;
end;

function TCWPlugin.GetParameterName(paramIndex: integer): string;
begin
  result := '';
  if (paramIndex >=0) and (paramIndex < FParameters.Count) then
    result := self.Parameter[paramIndex].ParamName;
end;

procedure TCWPlugin.GetPluginTypeFromRegistry;
const
  C_KEY='Software\Cakewalk Music Software\Cakewalk\Cakewalk VST X64\Inventory';
{$IFNDEF CPU64}
  _KEY_WOW64_64KEY = $0100;
{$ENDIF}
var
  RegistryEntry: TRegistry;
  i : integer;
  pluginKey : string;
  pluginClass : string;
begin
  {$IFNDEF CPU64}
  RegistryEntry := TRegistry.Create(KEY_READ or _KEY_WOW64_64KEY);
  {$ELSE}
  RegistryEntry := TRegistry.Create(KEY_READ);
  {$ENDIF}
  try
    RegistryEntry.RootKey := HKEY_CURRENT_USER;
    for i := 0 to RegistryPluginList.Count-1 do
    begin
      pluginKey := C_KEY + '\' + RegistryPluginList[i];
      if (RegistryEntry.OpenKey(pluginKey, false)) then
      begin
        pluginClass := StringReplace(StringReplace(RegistryEntry.ReadString('clsidPlug'),'{','C_',[]),'}','',[]);

        if (LowerCase(pluginClass) = LowerCase(FClassID)) then
        begin
          FIsSynth := (RegistryEntry.ReadInteger('isSynth') > 0);
          FIsVST3 := (RegistryEntry.ReadInteger('isVst3') > 0);
          FIsX64 := (RegistryEntry.ReadInteger('isX64') > 0);

          if (FIsX64) then  // keep looking if not 64 bit
            break;
        end;
        RegistryEntry.CloseKey;
      end;
    end;
  finally
    RegistryEntry.Free;
  end;
end;

procedure TCWPlugin.SetClassID(const Value: string);
begin
  FClassID := Value;
  GetPluginTypeFromRegistry;
end;

procedure TCWPlugin.SetName(const Value: string);
begin
  FName := Value;
end;

procedure GetRegistryPluginList;
const
  C_KEY='Software\Cakewalk Music Software\Cakewalk\Cakewalk VST X64\Inventory';
  {$IFNDEF WIN64}
  _KEY_WOW64_64KEY = $0100;
  {$ENDIF}
var
  RegistryEntry: TRegistry;
begin
  {$IFNDEF WIN64}
  RegistryEntry := TRegistry.Create(KEY_READ or _KEY_WOW64_64KEY);
  {$ELSE}
  RegistryEntry := TRegistry.Create(KEY_READ);
  {$ENDIF}
  try
    RegistryEntry.RootKey := HKEY_CURRENT_USER;
    if (RegistryEntry.OpenKey(C_KEY, false)) and (RegistryEntry.HasSubKeys) then
      RegistryEntry.GetKeyNames(RegistryPluginList);
    RegistryEntry.CloseKey();
  finally
    RegistryEntry.Free;
  end;
end;

procedure TCWPlugin.SortParameters;
begin
  FParameters.Sort(@CompareCWParameterIds);
end;

{ TCWPluginList }

procedure TCWPluginList.ParseProgress(Sender: TObject; CurrentIndex, FileSize: integer);
var
  pc : double;
begin
  if (FProgressBar <> nil) then
  begin
    pc := ((100 / Filesize) * CurrentIndex);
    FProgressBar.Position := round(pc);
    Application.ProcessMessages;
  end;
end;

procedure CWPluginListReset;
begin
  FreeAndNil(CWPluginList);
  FreeAndNil(RegistryPluginList);
  CWPluginList := TCWPluginList.Create(true);
  RegistryPluginList := TStringList.Create;
  GetRegistryPluginList;
end;

initialization
begin
  CWPluginList := TCWPluginList.Create(true);
  RegistryPluginList := TStringList.Create;
  GetRegistryPluginList;
end;

finalization
begin
  FreeAndNil(CWPluginList);
  FreeAndNil(RegistryPluginList);
end;

end.
