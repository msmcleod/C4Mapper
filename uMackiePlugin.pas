unit uMackiePlugin;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TMackiePluginParameterDataType = (pdtLevel, pdtPan, pdtFreq, pdtSwitch, pdtBoostCut, pdtSpread);
  TMackiePluginType = (ptGeneral, ptEQ, ptDynamics);

const
  MACKIE_PARAMETER_DATA_TYPES : array[TMackiePluginParameterDataType] of string = ('level','pan','freq','switch','boost/cut','spread');

type

  { TMackiePluginParameter }

  TMackiePluginParameter = class(TObject)
  private
    FHasDefaultValue: boolean;
    FHasIncrement: boolean;
    FDefaultValue: Double;
    FIncrement: Double;
    FControlName: string;
    FParamNumber: integer;
    FDataType: TMackiePluginParameterDataType;
    FIsAssigned: boolean;
    FIsEQAssigned: boolean;
    FEQVPot: string;
    FParameterName: string;
    FOriginalComment : string;
    FDisplayLabel : string;
    FControlLabelName : string;
    FOriginalLabelComment : string;
    procedure SetControlLabelName(AValue: string);
    procedure SetControlName(const Value: string);
    procedure SetDataType(const Value: TMackiePluginParameterDataType);
    procedure SetDefaultValue(const Value: Double);
    procedure SetHasDefaultValue(const Value: boolean);
    procedure SetHasIncrement(const Value: boolean);
    procedure SetIncrement(const Value: Double);
    procedure SetOriginalLabelComment(AValue: string);
    procedure SetParamNumber(const Value: integer);
    procedure SetIsAssigned(const Value: boolean);
    procedure SetEQVPot(const Value: string);
    procedure SetIsEQAssigned(const Value: boolean);
    procedure SetParameterName(const Value: string);
    procedure SetOriginalComment(const Value: string);
  public
    constructor Create;
    procedure CopyFrom(aParam : TMackiePluginParameter);
    property ControlName : string read FControlName write SetControlName;
    property ParameterName : string read FParameterName write SetParameterName;
    property ParamNumber : integer read FParamNumber write SetParamNumber;
    property DataType : TMackiePluginParameterDataType read FDataType write SetDataType;
    property DefaultValue : Double read FDefaultValue write SetDefaultValue;
    property Increment : Double read FIncrement write SetIncrement;
    property HasDefaultValue : boolean read FHasDefaultValue write SetHasDefaultValue;
    property HasIncrement : boolean read FHasIncrement write SetHasIncrement;
    property IsAssigned : boolean read FIsAssigned write SetIsAssigned;
    property IsEQAssigned : boolean read FIsEQAssigned write SetIsEQAssigned;
    property EQVPot : string read FEQVPot write SetEQVPot;
    property OriginalComment : string read FOriginalComment write SetOriginalComment;
    property DisplayLabel : string read FDisplayLabel write FDisplayLabel;
    property ControlLabelName : string read FControlLabelName write SetControlLabelName;
    property OriginalLabelComment : string read FOriginalLabelComment write SetOriginalLabelComment;
  end;

  { TMackieC4Plugin }

  TMackieC4Plugin = class(TObject)
  private
    FDisplayLabel: string;
    FPluginName: string;
    FNumVPots : integer;
    FNumFreqBands : integer;
    FPluginType : TMackiePluginType;
    FControls : array[0..4, 0..31] of TMackiePluginParameter;
    procedure SetPluginName(const Value: string);
    function GetPluginParameter(Index: string): TMackiePluginParameter;
    function GetPluginParameterByParamNo(Index: integer): TMackiePluginParameter;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateIniSection : string;
    function IsParameterAssigned(paramNo : integer) : boolean;
    function IndexesFromName(paramName : string; var mIndex, pIndex : integer) : boolean;
    procedure SetParameterIniValues(paramName, paramValues : string);
    procedure SetParameterEQIniValues(paramName, paramValues : string);
    procedure SetParameterNameIniValues(paramName, paramValues : string);
    function GetParameterByIndexes(mIndex, pIndex : integer) : TMackiePluginParameter;
    procedure PopulateMissingParamNamesWithOriginalComment;
    function AssignedCount : integer;
    property PluginName : string read FPluginName write SetPluginName;
    property Parameter[Index : string] : TMackiePluginParameter read GetPluginParameter;
    property ParameterByParamNo[Index : integer] : TMackiePluginParameter read GetPluginParameterByParamNo;
    property NumVPots : integer read FNumVPots write FNumVPots;
    property NumFreqBands : integer read FNumFreqBands write FNumFreqBands;
    property PluginType : TMackiePluginType read FPluginType write FPluginType;
    property DisplayLabel : string read FDisplayLabel write FDisplayLabel;
  end;

  TMackieC4PluginList = class(TObjectList)
  private
    FOwnsObjects: Boolean;
    function GetItemByName(Index: String): TMackieC4Plugin;
  protected
    function GetItem(Index: Integer): TMackieC4Plugin;
    procedure SetItem(Index: Integer; AObject: TMackieC4Plugin);
  public
    HeadingText : string;
    GuidanceText : string;
    function GenerateIniHeaderSection : string;
    function GenerateIni : string;
    function Add(AObject: TMackieC4Plugin): Integer;
    function Extract(Item: TMackieC4Plugin): TObject;
    function Remove(AObject: TMackieC4Plugin): Integer;
    function IndexOf(AObject: TMackieC4Plugin): Integer;
    procedure Insert(Index: Integer; AObject : TMackieC4Plugin);
    function First: TMackieC4Plugin;
    function Last: TMackieC4Plugin;
    function IndexOfName(PluginName : string) : Integer;
    procedure DeletePlugin(plugin : TMackieC4Plugin);
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property ItemsByIndex[Index: Integer]: TMackieC4Plugin read GetItem write SetItem;
    property Items[Index: String]: TMackieC4Plugin read GetItemByName; default;
  end;

  TMackieIniFile = class
  private
    FRawIniFile : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetComment(Section,Parameter : string) : string;
  end;

var
  MackiePluginList : TMackieC4PluginList;

procedure LoadMackiePlugins;
function GetMackieIniFilename : string;
function GetSurfacesDirectory : string;
procedure MackiePluginsReset;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  inifiles, Registry;

var
  C4VPotNames : TStringList;

function GetSurfacesDirectory : string;
const
  C_KEY='SOFTWARE\Cakewalk Music Software\Shared Surfaces';
  {$IFNDEF WIN64}
  _KEY_WOW64_64KEY = $0100;
  {$ENDIF}
var
  RegistryEntry: TRegistry;
begin
  result := 'C:\Program Files\Cakewalk\Shared Surfaces';
  {$IFNDEF WIN64}
  RegistryEntry := TRegistry.Create(KEY_READ or _KEY_WOW64_64KEY);
  {$ELSE}
  RegistryEntry := TRegistry.Create(KEY_READ);
  {$ENDIF}
  try
    RegistryEntry.RootKey := HKEY_LOCAL_MACHINE;
    if (RegistryEntry.OpenKey(C_KEY, false)) then
      result := RegistryEntry.ReadString('InstallDirectory');
    RegistryEntry.CloseKey();
  finally
    RegistryEntry.Free;
  end;
end;

function GetMackieIniFilename : string;
begin
  result := GetSurfacesDirectory + '\MackieControl.ini';
end;

procedure LoadMackiePlugins;
var
  f : TIniFile;
  section : TStringList;
  pluginlist : TStringList;
  i,j : integer;
  c4plugin : TMackieC4Plugin;
  pluginname,params,paramName : string;
  iVpots, err : integer;
  sectionraw : TStringList;
  MackieFilename : string;
begin
  MackiePluginList.Clear;
  f := nil;
  section := TStringList.Create;
  pluginlist := TStringList.Create;
  sectionraw := TStringList.Create;
  try
    try
      MackieFilename := GetMackieIniFilename;
      f := TIniFile.Create(MackieFilename);
      if f.SectionExists('plugins') then
      begin
        f.ReadSectionValues('plugins',pluginlist);
        for i := 0 to pluginlist.Count-1 do
        begin
          pluginname := trim(pluginlist.ValueFromIndex[i]);

          f.ReadSectionValues(pluginname,section);
          f.ReadSection(pluginname,sectionraw);

          c4plugin := TMackieC4Plugin.Create;
          c4plugin.PluginName := pluginname;

          for j := 0 to section.Count-1 do
          begin
            paramName := trim(section.Names[j]);
            params := StringReplace(trim(section.ValueFromIndex[j]),#9,' ',[rfReplaceAll]);

            if (paramName = 'PluginType') then
            begin
              if params = '1' then
                c4plugin.PluginType := ptEQ
              else if params = '2' then
                c4plugin.PluginType := ptDynamics
              else
                c4plugin.PluginType := ptGeneral;
            end
            else if (paramName = 'NumVPots') then
            begin
              Val(params,iVpots,err);
              if (err = 0) and (iVPots > 0) and (iVpots < 33) then
                c4plugin.NumVPots := iVpots
              else
                c4plugin.NumVPots := 32;
            end
            else if (paramName = 'NumFreqBands') then
            begin
              Val(params,iVpots,err);
              if (err = 0) and (iVPots > 0) and (iVpots < 33) then
                c4plugin.NumFreqBands := iVpots
              else
                c4plugin.NumFreqBands := 0;
            end
            else if (pos('VPotLabel',paramName) > 0) then
            begin
              c4plugin.SetParameterNameIniValues(paramName, params);
            end
            else if (Pos('VPot',paramName) > 0) then
              c4plugin.SetParameterIniValues(paramName, params)
            else
              c4plugin.SetParameterEQIniValues(paramName, params);
          end;

          MackiePluginList.Add(c4plugin);
        end;
      end;
    except
    end;
  finally
    FreeAndNil(section);
    FreeAndNil(pluginlist);
    FreeAndNil(sectionraw);
    FreeAndNil(f);
  end;
end;

{ TMackiePluginParameter }

procedure TMackiePluginParameter.CopyFrom(aParam: TMackiePluginParameter);
begin
  ParamNumber := aParam.ParamNumber;
  ParameterName := aParam.ParameterName;
  HasDefaultValue := aParam.HasDefaultValue;
  HasIncrement := aParam.HasIncrement;
  if (HasDefaultValue) then
    DefaultValue := aParam.DefaultValue;

  if (HasIncrement) then
    HasIncrement := aParam.HasIncrement;

  DataType := aParam.DataType;
  OriginalComment := aParam.OriginalComment;
  DisplayLabel := aParam.DisplayLabel;

  IsEQAssigned := aParam.IsEQAssigned;
  EQVPot := aParam.EQVPot;
end;

constructor TMackiePluginParameter.Create;
begin
  FControlName := 'VPot0';
  FControlLabelName := 'VPotLabel0';
  FParamNumber := -1;
  FParameterName := '';
  FHasDefaultValue := false;
  FHasIncrement := false;
  FDefaultValue := 0;
  FIncrement := 0;
  FDataType := pdtLevel;
  FIsAssigned := false;
  FOriginalComment := '';
  FIsEQAssigned := false;
  FEQVPot := '';
end;

function TMackieC4Plugin.IsParameterAssigned(paramNo: integer): boolean;
var
  i,j : integer;
begin
  result := false;
  for i := 0 to 4 do
  begin
    for j := 0 to 31 do
    begin
      if (FControls[i,j].ParamNumber = paramNo) and FControls[i,j].IsAssigned then
      begin
        result := true;
        break;
      end;
    end;
  end;
end;

procedure TMackiePluginParameter.SetControlName(const Value: string);
begin
  FControlName := Value;
end;

procedure TMackiePluginParameter.SetControlLabelName(AValue: string);
begin
  FControlLabelName:=AValue;
end;

procedure TMackiePluginParameter.SetDataType(const Value: TMackiePluginParameterDataType);
begin
  FDataType := Value;
end;

procedure TMackiePluginParameter.SetDefaultValue(const Value: Double);
begin
  FDefaultValue := Value;
end;

procedure TMackiePluginParameter.SetEQVPot(const Value: string);
begin
  FEQVPot := Value;
end;

procedure TMackiePluginParameter.SetHasDefaultValue(const Value: boolean);
begin
  FHasDefaultValue := Value;
  if (FHasDefaultValue = false) then
    FDefaultValue := 0;
end;

procedure TMackiePluginParameter.SetHasIncrement(const Value: boolean);
begin
  FHasIncrement := Value;
  if (FHasIncrement = false) then
    FIncrement := 0;
end;

procedure TMackiePluginParameter.SetIncrement(const Value: Double);
begin
  FIncrement := Value;
end;

procedure TMackiePluginParameter.SetOriginalLabelComment(AValue: string);
begin
  FOriginalLabelComment:=AValue;
end;

procedure TMackiePluginParameter.SetIsAssigned(const Value: boolean);
begin
  FIsAssigned := Value;
  if (FIsAssigned = false) then
  begin
    FParamNumber := -1;
    FParameterName := '';
    FHasDefaultValue := false;
    FHasIncrement := false;
    FDefaultValue := 0;
    FIncrement := 0;
    FDataType := pdtLevel;
    FIsEQAssigned := false;
    FEQVPot := '';
    FOriginalComment := '';
  end;
end;

procedure TMackiePluginParameter.SetIsEQAssigned(const Value: boolean);
begin
  FIsEQAssigned := Value;
end;

procedure TMackiePluginParameter.SetOriginalComment(const Value: string);
begin
  FOriginalComment := Value;
end;

procedure TMackiePluginParameter.SetParameterName(const Value: string);
begin
  FParameterName := Value;
end;

procedure TMackiePluginParameter.SetParamNumber(const Value: integer);
begin
  FParamNumber := Value;
end;

{ TMackieC4Plugin }

constructor TMackieC4Plugin.Create;
var
  i, j : integer;
begin
  inherited;

  for i := 0 to 4 do
    for j := 0 to 31 do
    begin
      FControls[i,j] := TMackiePluginParameter.Create;
      if (i > 0) then
      begin
        FControls[i,j].ControlName := 'M' + inttostr(i) + 'VPot' + inttostr(j);
        FControls[i,j].ControlLabelName := 'M' + inttostr(i) + 'VPotLabel' + inttostr(j);
      end
      else
      begin
        FControls[i,j].ControlName := 'VPot' + inttostr(j);
        FControls[i,j].ControlLabelName := 'VPotLabel' + inttostr(j);
      end;
    end;
end;

destructor TMackieC4Plugin.Destroy;
var
  i, j : integer;
begin
  for i := 0 to 4 do
    for j := 0 to 31 do
      FreeAndNil(FControls[i,j]);

  inherited;
end;

procedure TMackieC4Plugin.SetPluginName(const Value: string);
begin
  FPluginName := Value;
end;

procedure TMackieC4Plugin.SetParameterIniValues(paramName, paramValues : string);
var
  param : TMackiePluginParameter;
  sl : TStringList;
  err : integer;
  iVal : integer;
  fVal : double;
  s : string;
begin
  param := GetPluginParameter(paramName);
  if (param <> nil) then
  begin
    sl := TStringList.Create;
    try
      try
        param.ControlName := paramName;

        sl.CommaText := paramValues;
        if (sl.Count > 0) then
        begin
          Val(trim(sl[0]), iVal, err);
          if (err = 0) then
            param.ParamNumber := iVal;
        end;

        if (sl.Count > 1) then
        begin
          s := trim(lowercase(sl[1]));
          if (s = 'level') then
            param.DataType := pdtLevel
          else if (s = 'pan') then
            param.DataType := pdtPan
          else if (s = 'freq') then
            param.DataType := pdtFreq
          else if (s = 'switch') then
            param.DataType := pdtSwitch
          else if (s = 'boost/cut') then
            param.DataType := pdtBoostCut
          else if (s = 'spread') then
            param.DataType := pdtSpread;
        end;

        param.HasDefaultValue := false;
        param.HasIncrement := false;

        if (sl.Count > 2) then
        begin
          Val(trim(sl[2]), fVal, err);
          if (err = 0) then
          begin
            param.DefaultValue := fVal;
            param.HasDefaultValue := true;
          end;
        end;

        if (sl.Count > 3) then
        begin
          Val(trim(sl[3]), fVal, err);
          if (err = 0) then
          begin
            param.Increment := fVal;
            param.HasIncrement := true;
          end;
        end;

        if (param.ParamNumber >= 0) then
          param.IsAssigned := true;
      except
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TMackieC4Plugin.SetParameterEQIniValues(paramName, paramValues: string);
var
  iParamNo : integer;
  param : TMackiePluginParameter;
  sl : TStringList;
  err : integer;
  iVal : integer;
  fVal : double;
  s : string;
begin
  sl := TStringList.Create;
  try
    try
      iParamNo := -2;
      sl.CommaText := paramValues;
      if (sl.Count > 0) then
      begin
        Val(trim(sl[0]), iVal, err);
        if (err = 0) then
          iParamNo := iVal;
      end;

      param := GetPluginParameterByParamNo(iParamNo);
      if (param <> nil) then
      begin
        param.EQVPot := paramName;

        if (sl.Count > 1) then
        begin
          s := trim(lowercase(sl[1]));
          if (s = 'level') then
            param.DataType := pdtLevel
          else if (s = 'pan') then
            param.DataType := pdtPan
          else if (s = 'freq') then
            param.DataType := pdtFreq
          else if (s = 'switch') then
            param.DataType := pdtSwitch
          else if (s = 'boost/cut') then
            param.DataType := pdtBoostCut
          else if (s = 'spread') then
            param.DataType := pdtSpread;
        end;

        if (sl.Count > 2) then
        begin
          Val(trim(sl[2]), fVal, err);
          if (err = 0) then
          begin
            param.DefaultValue := fVal;
            param.HasDefaultValue := true;
          end;
        end;

        if (sl.Count > 3) then
        begin
          Val(trim(sl[3]), fVal, err);
          if (err = 0) then
          begin
            param.Increment := fVal;
            param.HasIncrement := true;
          end;
        end;
      end;

    except
    end;
  finally
    sl.Free;
  end;
end;

procedure TMackieC4Plugin.SetParameterNameIniValues(paramName, paramValues: string);
var
  param : TMackiePluginParameter;
  sl : TStringList;
  err : integer;
  iVal : integer;
  s : string;
  i : integer;
  p : integer;
begin
  param := GetPluginParameter(paramName);
  if (param <> nil) then
  begin
    sl := TStringList.Create;
    try
      try
        param.ControlName := StringReplace(paramName,'Label','',[rfReplaceAll]);
        param.ControlLabelName := paramName;

        sl.CommaText := paramValues;
        if (sl.Count > 0) then
        begin
          Val(trim(sl[0]), iVal, err);
          if (err = 0) then
            param.ParamNumber := iVal;
        end;

        if (sl.Count > 1) then
        begin
          s := '';
          p := pos(',',paramValues);
          if (p > 0) then
          begin
            s := Copy(paramValues,p+1,(length(paramValues) - (p+1)));
            i := length(s);
            if (i > 6) then i := 6;
            param.DisplayLabel := trim(Copy(s,1,i));
          end;
        end;

        if (param.ParamNumber >= 0) then
          param.IsAssigned := true;
      except
      end;
    finally
      sl.Free;
    end;
  end;
end;


function TMackieC4Plugin.GetPluginParameter(Index: string): TMackiePluginParameter;
var
  mIndex, pIndex : integer;
begin
  pIndex := -1;
  mIndex := -1;
  result := nil;
  if IndexesFromName(Index, mIndex, pIndex) then
    result := FControls[mIndex,pIndex]; 
end;

function TMackieC4Plugin.IndexesFromName(paramName: string; var mIndex, pIndex: integer): boolean;
var
  vPotPos : integer;
  valStr : string;
  err : integer;
begin
  result := false;
  mIndex := -1;
  pIndex := -1;
  vPotPos := Pos('VPot', paramName);
  if (vPotPos > 0) then
  begin
    paramName := StringReplace(paramName, 'Label', '', [rfReplaceAll]);
    valStr := Copy(paramName, vPotPos+4, length(paramName) - (vPotPos+3));

    if (vPotPos = 1) then
      mIndex := 0
    else
      mIndex := ord(paramName[2])-48;

    if (mIndex > 4) or (mIndex < 0) then
      exit;

    Val(valStr, pIndex, err);

    if err<>0 then
      exit;

    result := true;
  end;
end;

function TMackieC4Plugin.GetPluginParameterByParamNo(Index: integer): TMackiePluginParameter;
var
  m,p : integer;
begin
  result := nil;
  for m := 0 to 4 do
    for p := 0 to 31 do
      if (FControls[m,p].ParamNumber = Index) then
      begin
        result := FControls[m,p];
        exit;
      end;
end;

function TMackieC4Plugin.GetParameterByIndexes(mIndex, pIndex: integer): TMackiePluginParameter;
begin
  result := nil;
  if (mIndex >= 0) and (mIndex < 5) and (pIndex >=0) and (pIndex < 32) then
    result := FControls[mIndex, pIndex];
end;

function TMackieC4Plugin.GenerateIniSection: string;
const
  eqstr : array[1..5] of string = ('Gain','CourseFreq','FineFreq','Q','BandEnable');
var
  sl : TStringList;
  m,p : integer;
  i,j : integer;
  param : TMackiePluginParameter;
  s : string;
  eqStrings : TStringList;
begin
  result := '';
  eqStrings := TStringList.Create;
  sl := TStringList.Create;
  try
    sl.Add('[' + PluginName + ']');
    sl.Add('PluginType=' + inttostr(ord(PluginType)));
    sl.Add('NumVPots=' + inttostr(NumVPots));
    for m := 0 to 4 do
      for p := 0 to 31 do
      begin
        param := GetParameterByIndexes(m,p);
        if (param.IsAssigned) then
        begin
          s := param.ControlName + '=' + inttostr(param.ParamNumber) + ',' + MACKIE_PARAMETER_DATA_TYPES[param.DataType];
          if (param.HasDefaultValue) then
          begin
            s := s + ',' + FloatToStr(param.DefaultValue);
            if (param.HasIncrement) then
              s := s + ',' + FloatToStr(param.Increment);
          end;

          if param.ParameterName <> '' then
            s := s + #9 + #9 + '; ' + param.ParameterName
          else if param.OriginalComment <> '' then
            s := s + #9 + #9 + ';' + param.OriginalComment;

          sl.Add(s);
        end;
      end;

    if (PluginType = ptEQ) then
    begin
      sl.Add('NumFreqBands=' + inttostr(NumFreqBands));
      eqStrings.AddObject('XXXX',nil);

      for i := 1 to 5 do
        for j := 1 to NumFreqBands do
          eqStrings.AddObject(eqstr[i] + inttostr(j-1), nil);

      for m := 0 to 4 do
        for p := 0 to 31 do
        begin
          param := GetParameterByIndexes(m,p);
          if (param.IsAssigned) and (param.EQVPot <> '') then
          begin
            j := eqStrings.IndexOf(param.EQVPot);
            if (j > 0) then
              eqStrings.Objects[j] := param;
          end;
        end;

      for i := 1 to eqStrings.Count-1 do // skip first item as it contains XXXX
      begin
        if (eqStrings.Objects[i] <> nil) then
        begin
          param := TMackiePluginParameter(eqStrings.Objects[i]);
          s := param.EQVPot + '=' + inttostr(param.ParamNumber) + ',' + MACKIE_PARAMETER_DATA_TYPES[param.DataType];
          if (param.HasDefaultValue) then
          begin
            s := s + ',' + FloatToStr(param.DefaultValue);
            if (param.HasIncrement) then
              s := s + ',' + FloatToStr(param.Increment);
          end;

          if param.ParameterName <> '' then
            s := s + #9 + #9 + '; ' + param.ParameterName
          else if param.OriginalComment <> '' then
            s := s + #9 + #9 + ';' + param.OriginalComment;

          sl.Add(s);
        end
        else
          sl.Add(eqStrings[i] + '=');
      end;
    end;

    // VPot Param Labels
    for m := 0 to 4 do
      for p := 0 to 31 do
      begin
        param := GetParameterByIndexes(m,p);
        if (param.IsAssigned) and (param.DisplayLabel <> '') then
        begin
          s := param.ControlLabelName + '=' + inttostr(param.ParamNumber) + ',' + param.DisplayLabel;

          if param.ParameterName <> '' then
            s := s + #9 + #9 + '; ' + param.ParameterName
          else if param.OriginalLabelComment <> '' then
            s := s + #9 + #9 + ';' + param.OriginalComment;

          sl.Add(s);
        end;
      end;

    sl.Add('');
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TMackieC4Plugin.PopulateMissingParamNamesWithOriginalComment;
var
  m, p : integer;
  FIni : TMackieIniFile;
begin
  FIni := TMackieIniFile.Create;
  try
    for m := 0 to 4 do
      for p := 0 to 31 do
      begin
        if (FControls[m,p].IsAssigned) and (FControls[m,p].ParameterName = '') then
        begin
          FControls[m,p].OriginalComment := FIni.GetComment(PluginName, FControls[m,p].ControlName);
          FControls[m,p].OriginalLabelComment := FIni.GetComment(PluginName, FControls[m,p].ControlLabelName);
        end;
      end;
  finally
    FreeAndNil(FIni);
  end;
end;

function TMackieC4Plugin.AssignedCount: integer;
var
  m,p : integer;
begin
  result := 0;
  for m := 0 to 4 do
    for p := 0 to 31 do
      if FControls[m,p].IsAssigned then
        result := result + 1;
end;

{ TMackieC4PluginList }

function TMackieC4PluginList.Add(AObject: TMackieC4Plugin): Integer;
begin
  result := inherited Add(AObject);
end;

procedure TMackieC4PluginList.DeletePlugin(plugin: TMackieC4Plugin);
var
  i : integer;
begin
  i := Self.IndexOf(plugin);
  if (i >= 0) then
    Delete(i);
end;

function TMackieC4PluginList.Extract(Item: TMackieC4Plugin): TObject;
begin
  result := TMackieC4Plugin(inherited Extract(Item));
end;

function TMackieC4PluginList.First: TMackieC4Plugin;
begin
  result := TMackieC4Plugin(inherited First);
end;

function TMackieC4PluginList.GenerateIni: string;
var
  sl : TStringList;
  i : integer;
begin
  result := '';
  sl := TStringList.Create;
  try
    sl.Add(HeadingText);
    sl.Add(GenerateIniHeaderSection);
    sl.Add(GuidanceText);
    for i := 0 to Count-1 do
      sl.Add(ItemsByIndex[i].GenerateIniSection);

    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TMackieC4PluginList.GenerateIniHeaderSection: string;
var
  i : integer;
  sl : TStringList;
begin
  result := '';
  sl := TStringList.Create;
  try
    sl.Add('[Plugins]');
    for i := 0 to Count-1 do
    begin
      sl.Add(inttostr(i) + '=' + ItemsByIndex[i].PluginName);
    end;

    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TMackieC4PluginList.GetItem(Index: Integer): TMackieC4Plugin;
begin
  result := TMackieC4Plugin(inherited GetItem(Index));
end;

function TMackieC4PluginList.GetItemByName(Index: String): TMackieC4Plugin;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count-1 do
    if Self.ItemsByIndex[i].PluginName = Index then
    begin
      result := ItemsByIndex[i];
      exit;
    end;
end;

function TMackieC4PluginList.IndexOf(AObject: TMackieC4Plugin): Integer;
begin
  result := inherited IndexOf(AObject);
end;

function TMackieC4PluginList.IndexOfName(PluginName: string): Integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to Count-1 do
    if Self.ItemsByIndex[i].PluginName = PluginName then
    begin
      result := i;
      exit;
    end;
end;

procedure TMackieC4PluginList.Insert(Index: Integer; AObject: TMackieC4Plugin);
begin
  inherited Insert(Index, AObject);
end;

function TMackieC4PluginList.Last: TMackieC4Plugin;
begin
  result := TMackieC4Plugin(inherited Last);
end;

function TMackieC4PluginList.Remove(AObject: TMackieC4Plugin): Integer;
begin
  result := inherited Remove(AObject);
end;

procedure TMackieC4PluginList.SetItem(Index: Integer; AObject: TMackieC4Plugin);
begin
  inherited SetItem(Index, AObject);
end;

procedure CreateAndPopulateC4VPotNames;
var
  i,j : integer;
begin
  C4VPotNames := TStringList.Create;
  for i := 0 to 4 do
    for j := 0 to 31 do
    begin
      if i>0 then
        C4VPotNames.Add('VPot' + inttostr(j))
      else
        C4VPotNames.Add('M' + inttostr(i) + 'VPot' + inttostr(j));
    end;
end;

{ TMackieIniFile }

constructor TMackieIniFile.Create;
begin
  FRawIniFile := TStringList.Create;
  if FileExists(GetMackieIniFilename) then
    FRawIniFile.LoadFromFile(GetMackieIniFilename);
end;

destructor TMackieIniFile.Destroy;
begin
  FreeAndNil(FRawIniFile);
  inherited;
end;

function TMackieIniFile.GetComment(Section, Parameter: string): string;
var
  i : integer;
  findSection : string;
  paramValue : string;
  paramName : string;
  commentPos : integer;
begin
  result := '';
  i := 0;
  findSection := '[' + Section + ']';
  while (trim(FRawIniFile[i]) <> findSection) and (i < FRawIniFile.Count) do
    i := i + 1;

  if (i < FRawIniFile.Count) then
  begin
    i := i + 1;
    while (i < FRawIniFile.Count) do
    begin
      if (Pos('[', FRawIniFile[i]) = 1) then
        exit;

      paramName := FRawIniFile.Names[i];
      if (paramName = Parameter) then
      begin
        paramValue := FRawIniFile.ValueFromIndex[i];
        commentPos := Pos(';', paramValue);
        result := Copy(paramValue, commentPos + 1, (length(paramValue) - commentPos));
        exit;
      end;

      i := i + 1;
    end;
  end;
end;

procedure MackiePluginsReset;
begin
  FreeAndNil(MackiePluginList);
  FreeAndNil(C4VPotNames);

  MackiePluginList := TMackieC4PluginList.Create(true);
  CreateAndPopulateC4VPotNames;
end;

initialization
begin
  MackiePluginList := TMackieC4PluginList.Create(true);
  CreateAndPopulateC4VPotNames;
end;

finalization
begin
  FreeAndNil(MackiePluginList);
  FreeAndNil(C4VPotNames);
end;

end.
