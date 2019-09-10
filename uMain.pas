unit uMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Mask, Windows,
{$ELSE}
  MaskEdit, LCLIntf, LCLType,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, uMackiePlugin, Buttons, ComCtrls;

const
  WM_AFTER_SHOW = WM_USER + 300;

type

  TKeyPressMode = (kpmNone, kpmQ, kpmCoarseFreq, kpmFineFreq, kpmGain, kpmBandEnable);
  { TForm1 }

  TForm1 = class(TForm)
    btnKeyFocus: TButton;
    ebVPotDisplayLabel: TEdit;
    lblParamDisplayLabel: TLabel;
    txtKnobTextDisp1: TStaticText;
    txtKnobTextDisp10: TStaticText;
    txtKnobTextDisp11: TStaticText;
    txtKnobTextDisp12: TStaticText;
    txtKnobTextDisp13: TStaticText;
    txtKnobTextDisp14: TStaticText;
    txtKnobTextDisp15: TStaticText;
    txtKnobTextDisp16: TStaticText;
    txtKnobTextDisp17: TStaticText;
    txtKnobTextDisp18: TStaticText;
    txtKnobTextDisp19: TStaticText;
    txtKnobTextDisp2: TStaticText;
    txtKnobTextDisp20: TStaticText;
    txtKnobTextDisp21: TStaticText;
    txtKnobTextDisp22: TStaticText;
    txtKnobTextDisp23: TStaticText;
    txtKnobTextDisp24: TStaticText;
    txtKnobTextDisp25: TStaticText;
    txtKnobTextDisp26: TStaticText;
    txtKnobTextDisp27: TStaticText;
    txtKnobTextDisp28: TStaticText;
    txtKnobTextDisp29: TStaticText;
    txtKnobTextDisp3: TStaticText;
    txtKnobTextDisp30: TStaticText;
    txtKnobTextDisp31: TStaticText;
    txtKnobTextDisp32: TStaticText;
    txtKnobTextDisp4: TStaticText;
    txtKnobTextDisp5: TStaticText;
    txtKnobTextDisp6: TStaticText;
    txtKnobTextDisp7: TStaticText;
    txtKnobTextDisp8: TStaticText;
    txtKnobTextDisp9: TStaticText;
    VPotKnobImage: TImage;
    KnobImageList: TImageList;
    Panel1: TPanel;
    pnlConsole: TPanel;
    pnlProperties: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlList: TPanel;
    KnobText1: TStaticText;
    KnobText2: TStaticText;
    KnobText3: TStaticText;
    KnobText4: TStaticText;
    KnobText5: TStaticText;
    KnobText6: TStaticText;
    KnobText7: TStaticText;
    KnobText8: TStaticText;
    KnobText9: TStaticText;
    KnobText10: TStaticText;
    KnobText11: TStaticText;
    KnobText12: TStaticText;
    KnobText13: TStaticText;
    KnobText14: TStaticText;
    KnobText15: TStaticText;
    KnobText16: TStaticText;
    KnobText17: TStaticText;
    KnobText18: TStaticText;
    KnobText19: TStaticText;
    KnobText20: TStaticText;
    KnobText21: TStaticText;
    KnobText22: TStaticText;
    KnobText23: TStaticText;
    KnobText24: TStaticText;
    KnobText25: TStaticText;
    KnobText26: TStaticText;
    KnobText27: TStaticText;
    KnobText28: TStaticText;
    KnobText29: TStaticText;
    KnobText30: TStaticText;
    KnobText31: TStaticText;
    KnobText32: TStaticText;
    gbModifiers: TGroupBox;
    rbModifierNone: TRadioButton;
    rbModifierM1: TRadioButton;
    rbModifierM2: TRadioButton;
    rbModifierM3: TRadioButton;
    rbModifierM4: TRadioButton;
    gbPluginParameters: TGroupBox;
    lbParameters: TListBox;
    lblStatus: TLabel;
    gbPlugins: TGroupBox;
    lbPlugins: TListBox;
    Splitter1: TSplitter;
    gbControlProperties: TGroupBox;
    lblVPotName: TLabel;
    gbPluginProperties: TGroupBox;
    lblPluginName: TLabel;
    ebPluginName: TEdit;
    rgPluginType: TRadioGroup;
    lblPluginType: TLabel;
    lblNumVPots: TLabel;
    ebPluginVPots: TSpinEdit;
    ebPluginFreqBands: TSpinEdit;
    Label3: TLabel;
    ebVPotParamName: TEdit;
    rgVPotType: TRadioGroup;
    ebVPotIncrement: TMaskEdit;
    Label1: TLabel;
    cbVPotEQParam: TComboBox;
    Label2: TLabel;
    ebVPotName: TEdit;
    ebVPotDefault: TMaskEdit;
    cbVPotDefault: TCheckBox;
    cbVPotParamIncrement: TCheckBox;
    imgTrashCan: TImage;
    KnobImage1: TImage;
    KnobImage2: TImage;
    KnobImage3: TImage;
    KnobImage4: TImage;
    KnobImage5: TImage;
    KnobImage6: TImage;
    KnobImage7: TImage;
    KnobImage8: TImage;
    KnobImage9: TImage;
    KnobImage10: TImage;
    KnobImage11: TImage;
    KnobImage12: TImage;
    KnobImage13: TImage;
    KnobImage14: TImage;
    KnobImage15: TImage;
    KnobImage16: TImage;
    KnobImage17: TImage;
    KnobImage18: TImage;
    KnobImage19: TImage;
    KnobImage20: TImage;
    KnobImage21: TImage;
    KnobImage22: TImage;
    KnobImage23: TImage;
    KnobImage24: TImage;
    KnobImage25: TImage;
    KnobImage26: TImage;
    KnobImage27: TImage;
    KnobImage28: TImage;
    KnobImage29: TImage;
    KnobImage30: TImage;
    KnobImage31: TImage;
    KnobImage32: TImage;
    lbCurrentPluginName: TStaticText;
    txtInfo: TMemo;
    txtHeading: TMemo;
    txtGuidance: TMemo;
    btnSave: TSpeedButton;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    btnReload: TSpeedButton;
    txtDebug: TMemo;
    procedure btnKeyFocusKeyPress(Sender: TObject; var Key: char);
    procedure ebVPotDisplayLabelChange(Sender: TObject);
    procedure ebVPotDisplayLabelKeyPress(Sender: TObject; var Key: char);
    procedure KnobImageDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure KnobImageDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbParametersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbPluginsChange(Sender: TObject);
    procedure lbParametersMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ebPluginFreqBandsChange(Sender: TObject);
    procedure rgPluginTypeClick(Sender: TObject);
    procedure rgPluginTypeExit(Sender: TObject);
    procedure lbPluginsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbModifierClick(Sender: TObject);
    procedure imgTrashCanDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure KnobImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure KnobImageClick(Sender: TObject);
    procedure rgVPotTypeClick(Sender: TObject);
    procedure ebVPotDefaultChange(Sender: TObject);
    procedure ebVPotIncrementChange(Sender: TObject);
    procedure ebPluginVPotsChange(Sender: TObject);
    procedure imgTrashCanDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbPluginsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnSaveClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure cbVPotEQParamChange(Sender: TObject);
  private
    { Private declarations }
    FPluginsLoaded : boolean;
    FUpdatingDisplay : boolean;
    FLabels : array [0..31] of TStaticText;
    FKnobImages : array [0..31] of TImage;
    FDisplayLabels : array [0..31] of TStaticText;
    FCurrentMackiePlugin : TMackieC4Plugin;
    FCurrentMackiePluginParameter : TMackiePluginParameter;
    FCurrentParameterKnobIdx : integer;
    FCurrentKeyMode : TKeyPressMode;
    procedure LoadBitmaps;
    procedure EnableDisableEQControls;
    procedure LoadDataFiles;
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    function GetCurrentModifier : integer;
    procedure CopyImageFromImageList(imgIdx : integer; var img : TImage);
    procedure UpdateKnobImages;
    procedure UpdateDisplayLabel(modifier, index : integer);
  public
    { Public declarations }
    procedure UpdateDisplay;
  end;

var
  Form1: TForm1;

implementation

uses
  uCWPlugin, FolderHelper, uStringCruncher;

{$R *.dfm}

const
  IDX_BLANK_KNOB_IMAGE = 6;

{ TForm1 }

procedure TForm1.KnobImageDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  m, p, p2 : integer;
  param : TCWPluginParameter;
  okToContinue : boolean;
  sourcePluginParameter : TMackiePluginParameter;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  okToContinue := false;
  p := -1;

  if (Source = lbParameters) and (lbParameters.ItemIndex >= 0) and (lbPlugins.ItemIndex >= 0) then
  begin
    if (FCurrentMackiePlugin = nil) then
    begin
      FCurrentMackiePlugin := TMackieC4Plugin.Create;
      FCurrentMackiePlugin.PluginName := TCWPlugin(lbPlugins.Items.Objects[lbPlugins.ItemIndex]).Name;
      FCurrentMackiePlugin.NumVPots := 32;
      MackiePluginList.Add(FCurrentMackiePlugin);
    end;

    UpdateDisplay;

    if (Sender is TImage) and (Pos('KnobImage',TImage(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    begin
      p := TImage(Sender).Tag - 1;
      okToContinue := true;
    end;

    if (Sender is TStaticText) and (Pos('KnobText',TStaticText(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    begin
      p := TStaticText(Sender).Tag - 1;
      okToContinue := true;
    end;

    if (okToContinue) then
    begin
      m := GetCurrentModifier;
      FCurrentParameterKnobIdx := p;
      FCurrentMackiePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p);

      param := TCWPluginParameter(lbParameters.Items.Objects[lbParameters.ItemIndex]);

      FCurrentMackiePluginParameter.ParameterName := param.ParamName;
      FCurrentMackiePluginParameter.ParamNumber := param.ParamIndex;
      FCurrentMackiePluginParameter.HasDefaultValue := false;
      FCurrentMackiePluginParameter.HasIncrement := false;
      FCurrentMackiePluginParameter.IsAssigned := true;

      UpdateDisplay;
    end;

    exit;
  end;

  okToContinue := false;
  p2 := -1;

  if (Source is TImage) and (Pos('KnobImage',TImage(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p2 := TImage(Source).Tag-1;
    okToContinue := true;
  end
  else if (Source is TStaticText) and (Pos('KnobText',TStaticText(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p2 := TStaticText(Source).Tag-1;
    okToContinue := true;
  end;

  if (okToContinue) then
  begin
    okToContinue := false;

    if (Sender is TImage) and (Pos('KnobImage',TImage(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    begin
      p := TImage(Sender).Tag - 1;
      okToContinue := true;
    end;

    if (Sender is TStaticText) and (Pos('KnobText',TStaticText(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    begin
      p := TStaticText(Sender).Tag - 1;
      okToContinue := true;
    end;

    if (okToContinue) then
    begin
      m := GetCurrentModifier;
      sourcePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p2);
      FCurrentMackiePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p);

      if (sourcePluginParameter <> FCurrentMackiePluginParameter) then
      begin
        FCurrentMackiePluginParameter.CopyFrom(sourcePluginParameter);
        FCurrentMackiePluginParameter.IsAssigned := true;

        if (GetKeyState(VK_CONTROL) >= 0) then
          sourcePluginParameter.IsAssigned := false;
      end;

      UpdateDisplay;
    end;
  end;
end;
{$POP}

procedure TForm1.btnKeyFocusKeyPress(Sender: TObject; var Key: char);
var
  paramNum : integer;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  if (FCurrentMackiePluginParameter <> nil) then
  begin
    case Key of
      'L', 'l' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 0; // Level
          rgVPotTypeClick(Self);
        end;

      'P', 'p' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 1; // Pan
          rgVPotTypeClick(Self);
        end;

      'F', 'f' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 2; // Freq
          rgVPotTypeClick(Self);
        end;

      'S', 's' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 3; // Switch
          rgVPotTypeClick(Self);
        end;

      'B', 'b' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 4; // Boost/Cut
          rgVPotTypeClick(Self);
        end;

      'R', 'r' :
        begin
          FCurrentKeyMode := kpmNone;
          rgVPotType.ItemIndex := 5; // Sp[r]ead
          rgVPotTypeClick(Self);
        end;

      'Q', 'q' : if (rgPluginType.ItemIndex = 1) then FCurrentKeyMode := kpmQ else FCurrentKeyMode := kpmNone;
      'C', 'c' : if (rgPluginType.ItemIndex = 1) then FCurrentKeyMode := kpmCoarseFreq else FCurrentKeyMode := kpmNone;
      'N', 'n' : if (rgPluginType.ItemIndex = 1) then FCurrentKeyMode := kpmFineFreq else FCurrentKeyMode := kpmNone;
      'G', 'g' : if (rgPluginType.ItemIndex = 1) then FCurrentKeyMode := kpmGain else FCurrentKeyMode := kpmNone;
      'E', 'e' : if (rgPluginType.ItemIndex = 1) then FCurrentKeyMode := kpmBandEnable else FCurrentKeyMode := kpmNone;

      '0'..'9' :
        begin
          paramNum := Ord(Key) - Ord('0');
          if (rgPluginType.ItemIndex = 1) and (paramNum < ebPluginFreqBands.Value) then
          begin
            case FCurrentKeyMode of
              kpmQ : cbVPotEQParam.ItemIndex := cbVPotEQParam.Items.IndexOf('Q'+ Key);
              kpmCoarseFreq : cbVPotEQParam.ItemIndex := cbVPotEQParam.Items.IndexOf('CourseFreq'+ Key);
              kpmFineFreq : cbVPotEQParam.ItemIndex := cbVPotEQParam.Items.IndexOf('FineFreq'+ Key);
              kpmGain : cbVPotEQParam.ItemIndex := cbVPotEQParam.Items.IndexOf('Gain'+ Key);
              kpmBandEnable : cbVPotEQParam.ItemIndex := cbVPotEQParam.Items.IndexOf('BandEnable'+ Key);
            end;

            if (FCurrentKeyMode <> kpmNone) then
            begin
              cbVPotEQParamChange(Self);
              FCurrentKeyMode := kpmNone;
            end;
          end
          else
            FCurrentKeyMode := kpmNone;
        end;
    end;
  end
  else
    FCurrentKeyMode := kpmNone;
end;

procedure TForm1.ebVPotDisplayLabelChange(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  if (FUpdatingDisplay = false) and (FCurrentMackiePlugin <> nil) and (FCurrentMackiePluginParameter <> nil) then
  begin
    FCurrentMackiePluginParameter.DisplayLabel := trim(ebVPotDisplayLabel.Text);
    if (FCurrentParameterKnobIdx >= 0) then
    begin
        UpdateDisplayLabel(GetCurrentModifier, FCurrentParameterKnobIdx);
    end;
  end;
end;

procedure TForm1.ebVPotDisplayLabelKeyPress(Sender: TObject; var Key: char);
begin
  if (Key > ' ') then
  begin
    if (length(ebVPotDisplayLabel.Text) >= 6) then
    begin
      Key := #0;
      exit;
    end;

    if (not(Key in ['a'..'z','A'..'Z','0'..'9','-','_'])) then
      Key := #0;
  end;
end;

{$POP}

procedure TForm1.KnobImageDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
{$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  if (Source is TImage) and (Pos('KnobImage',TImage(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    accept := true
  else if (Source is TStaticText) and (Pos('KnobText',TStaticText(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
    accept := true
  else if (Source = lbParameters) and (lbParameters.ItemIndex >= 0) then
    Accept := true
  else
    Accept := false;
end;
{$POP}

procedure TForm1.lbParametersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  cwparam : TCWPluginParameter;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  with (Control as TListBox).Canvas do
  begin
    if (Index >= 0) then
    begin
      cwparam := TCWPluginParameter((Control as TListBox).Items.Objects[Index]);
      Font.Style := [];
      Font.Color := clDefault;
      if (FCurrentMackiePlugin <> nil) and (FCurrentMackiePlugin.IsParameterAssigned(cwparam.ParamIndex)) then
      begin
        Font.Style := [fsBold];
        Font.Color := clBlue;
      end;
    end;

    if (odSelected in State) then
      Brush.Color := clMoneyGreen
    else
      Brush.Color := (Control as TListBox).Color;

    FillRect(Rect);
    TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index]);

    if odFocused In State then
      DrawFocusRect(Rect);
  end;
end;
{$POP}

procedure TForm1.cbPluginsChange(Sender: TObject);
var
  i : integer;
  cwPlugin : TCWPlugin;
begin
  if not(FPluginsLoaded) then
    exit;

  if (lbPlugins.ItemIndex >= 0) then
  begin
    lbCurrentPluginName.Caption := lbPlugins.Items[lbPlugins.ItemIndex];
    lbParameters.Items.BeginUpdate;
    lbParameters.Items.Clear;
    cwPlugin := TCWPlugin(lbPlugins.Items.Objects[lbPlugins.ItemIndex]);
    FCurrentMackiePlugin := MackiePluginList[cwPlugin.Name];

    for i := 0 to cwPlugin.Parameters.Count-1 do
      lbParameters.AddItem(cwPlugin.Parameter[i].ParamName, cwPlugin.Parameter[i]);

    gbPluginParameters.Caption := 'Parameters (' + inttostr(cwPlugin.Parameters.Count) + ')';
    lbParameters.Items.EndUpdate;
    FCurrentMackiePluginParameter := nil;
  end
  else
  begin
    gbPluginParameters.Caption := 'Parameters';
    lbCurrentPluginName.Caption := '';
    lbParameters.Items.Clear;
    FCurrentMackiePlugin := nil;
    FCurrentMackiePluginParameter := nil;
  end;

  UpdateDisplay;
end;

procedure TForm1.lbParametersMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  if ssLeft in Shift then
    lbParameters.BeginDrag(true);
end;
{$POP}

procedure TForm1.ebPluginFreqBandsChange(Sender: TObject);
const
  eqstr : array[1..5] of string = ('Gain','CourseFreq','FineFreq','Q','BandEnable');
var
  i,j : integer;
  numBands : integer;
begin
  if not(FPluginsLoaded) then
    exit;

  try
    numBands := ebPluginFreqBands.Value;
  except
    on EConvertError do exit;
  end;
  cbVPotEQParam.Items.BeginUpdate;
  cbVPotEQParam.Items.Clear;
  cbVPotEQParam.Items.Add('');

  if (rgPluginType.ItemIndex = 1) then
  begin
    for i := 1 to 5 do
      for j := 1 to numBands do
        cbVPotEQParam.Items.Add(eqstr[i] + inttostr(j-1));
  end;

  cbVPotEQParam.Items.EndUpdate;

  if (FCurrentMackiePlugin <> nil) then
    FCurrentMackiePlugin.NumFreqBands := numBands;
end;

procedure TForm1.EnableDisableEQControls;
var
  eqEnable : boolean;
begin
  if not(FPluginsLoaded) then
    exit;

  eqEnable := (rgPluginType.ItemIndex = 1);

  if (eqEnable = false) then
  begin
    cbVPotEQParam.Text := '';
    ebPluginFreqBands.Value := 0;
  end;

  ebPluginFreqBands.Enabled := eqEnable;
  cbVPotEQParam.Enabled := eqEnable;
end;

procedure TForm1.rgPluginTypeClick(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  if (FCurrentMackiePlugin <> nil) and (rgPluginType.ItemIndex >=0) then
    FCurrentMackiePlugin.PluginType := TMackiePluginType(ord(rgPluginType.ItemIndex));

  EnableDisableEQControls;
end;

procedure TForm1.rgPluginTypeExit(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  EnableDisableEQControls;
end;

procedure TForm1.lbPluginsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  cwplugin : TCWPlugin;
  displayString : string;
  tempStr : string;
  pluginAssigned : boolean;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  cwplugin := nil;
  pluginAssigned := false;

  with (Control as TListBox).Canvas do
  begin
    if (Index >= 0) then
    begin
      cwplugin := TCWPlugin((Control as TListBox).Items.Objects[Index]);

      if (odSelected in State) then
        Brush.Color := clMoneyGreen
      else
        Brush.Color := (Control as TListBox).Color;

      if (MackiePluginList.IndexOfName(cwplugin.Name) >= 0) then
      begin
        pluginAssigned := true;
        Font.Style := [fsBold];
        Font.Color := clBlue;
      end
      else
      begin
        pluginAssigned := false;
        Font.Style := [];
        Font.Color := clDefault;
      end;
    end;

    FillRect(Rect);
    displayString := (Control as TListBox).Items[Index];

    if (cwPLugin <> nil) and (cwPlugin.IsVST3 = false) and (pluginAssigned = false) then
      Font.Color := clOlive;

    if (cwPlugin <> nil) and (cwPlugin.IsVST3 or cwPlugin.IsSynth or (not(cwPlugin.IsX64))) then
    begin
      tempStr := '';
      if (cwPlugin.IsVST3) and (Pos('vst3', displayString) = 0) then
        tempStr := 'vst3';

      if (cwPlugin.IsSynth) then
      begin
        if (tempStr <> '') then
          tempStr := tempStr + ',';

        tempStr := tempStr + 'synth';
      end;

      if not(cwPlugin.IsX64) then
      begin
        if (tempStr <> '') then
          tempStr := tempStr + ',';

        if (pluginAssigned = false) then
          Font.Color := clGreen;

        tempStr := tempStr + '32-BIT';
      end;

      if (tempStr <> '') then
        displayString := displayString + '  (' + tempStr + ')';
    end;

    TextOut(Rect.Left, Rect.Top, displayString);

    if odFocused In State then
      DrawFocusRect(Rect);
  end;

end;
{$POP}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurrentMackiePlugin := nil;
  FCurrentMackiePluginParameter := nil;
  FUpdatingDisplay := false;
  FPluginsLoaded := false;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Setup array of labels & knob images, so these can be easily accessed
  // using the "Tag" field of the control during handling of drag/drop events
  FLabels[0] := KnobText1;
  FLabels[1] := KnobText2;
  FLabels[2] := KnobText3;
  FLabels[3] := KnobText4;
  FLabels[4] := KnobText5;
  FLabels[5] := KnobText6;
  FLabels[6] := KnobText7;
  FLabels[7] := KnobText8;
  FLabels[8] := KnobText9;
  FLabels[9] := KnobText10;
  FLabels[10] := KnobText11;
  FLabels[11] := KnobText12;
  FLabels[12] := KnobText13;
  FLabels[13] := KnobText14;
  FLabels[14] := KnobText15;
  FLabels[15] := KnobText16;
  FLabels[16] := KnobText17;
  FLabels[17] := KnobText18;
  FLabels[18] := KnobText19;
  FLabels[19] := KnobText20;
  FLabels[20] := KnobText21;
  FLabels[21] := KnobText22;
  FLabels[22] := KnobText23;
  FLabels[23] := KnobText24;
  FLabels[24] := KnobText25;
  FLabels[25] := KnobText26;
  FLabels[26] := KnobText27;
  FLabels[27] := KnobText28;
  FLabels[28] := KnobText29;
  FLabels[29] := KnobText30;
  FLabels[30] := KnobText31;
  FLabels[31] := KnobText32;

  FDisplayLabels[0] := txtKnobTextDisp1;
  FDisplayLabels[1] := txtKnobTextDisp2;
  FDisplayLabels[2] := txtKnobTextDisp3;
  FDisplayLabels[3] := txtKnobTextDisp4;
  FDisplayLabels[4] := txtKnobTextDisp5;
  FDisplayLabels[5] := txtKnobTextDisp6;
  FDisplayLabels[6] := txtKnobTextDisp7;
  FDisplayLabels[7] := txtKnobTextDisp8;
  FDisplayLabels[8] := txtKnobTextDisp9;
  FDisplayLabels[9] := txtKnobTextDisp10;
  FDisplayLabels[10] := txtKnobTextDisp11;
  FDisplayLabels[11] := txtKnobTextDisp12;
  FDisplayLabels[12] := txtKnobTextDisp13;
  FDisplayLabels[13] := txtKnobTextDisp14;
  FDisplayLabels[14] := txtKnobTextDisp15;
  FDisplayLabels[15] := txtKnobTextDisp16;
  FDisplayLabels[16] := txtKnobTextDisp17;
  FDisplayLabels[17] := txtKnobTextDisp18;
  FDisplayLabels[18] := txtKnobTextDisp19;
  FDisplayLabels[19] := txtKnobTextDisp20;
  FDisplayLabels[20] := txtKnobTextDisp21;
  FDisplayLabels[21] := txtKnobTextDisp22;
  FDisplayLabels[22] := txtKnobTextDisp23;
  FDisplayLabels[23] := txtKnobTextDisp24;
  FDisplayLabels[24] := txtKnobTextDisp25;
  FDisplayLabels[25] := txtKnobTextDisp26;
  FDisplayLabels[26] := txtKnobTextDisp27;
  FDisplayLabels[27] := txtKnobTextDisp28;
  FDisplayLabels[28] := txtKnobTextDisp29;
  FDisplayLabels[29] := txtKnobTextDisp30;
  FDisplayLabels[30] := txtKnobTextDisp31;
  FDisplayLabels[31] := txtKnobTextDisp32;

  FKnobImages[0] := KnobImage1;
  FKnobImages[1] := KnobImage2;
  FKnobImages[2] := KnobImage3;
  FKnobImages[3] := KnobImage4;
  FKnobImages[4] := KnobImage5;
  FKnobImages[5] := KnobImage6;
  FKnobImages[6] := KnobImage7;
  FKnobImages[7] := KnobImage8;
  FKnobImages[8] := KnobImage9;
  FKnobImages[9] := KnobImage10;
  FKnobImages[10] := KnobImage11;
  FKnobImages[11] := KnobImage12;
  FKnobImages[12] := KnobImage13;
  FKnobImages[13] := KnobImage14;
  FKnobImages[14] := KnobImage15;
  FKnobImages[15] := KnobImage16;
  FKnobImages[16] := KnobImage17;
  FKnobImages[17] := KnobImage18;
  FKnobImages[18] := KnobImage19;
  FKnobImages[19] := KnobImage20;
  FKnobImages[20] := KnobImage21;
  FKnobImages[21] := KnobImage22;
  FKnobImages[22] := KnobImage23;
  FKnobImages[23] := KnobImage24;
  FKnobImages[24] := KnobImage25;
  FKnobImages[25] := KnobImage26;
  FKnobImages[26] := KnobImage27;
  FKnobImages[27] := KnobImage28;
  FKnobImages[28] := KnobImage29;
  FKnobImages[29] := KnobImage30;
  FKnobImages[30] := KnobImage31;
  FKnobImages[31] := KnobImage32;
  LoadBitmaps;
  UpdateDisplay;

  MackiePluginList.HeadingText := txtHeading.Text;
  MackiePluginList.GuidanceText := txtGuidance.Text;

  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);  
end;

procedure TForm1.LoadDataFiles;
var
  i,m,p : integer;
  mplug : TMackieC4Plugin;
  mparam : TMackiePluginParameter;
begin
  FPluginsLoaded := false;

  Screen.Cursor := crHourGlass;

  lbPlugins.Clear;
  lbParameters.Clear;

  LoadMackiePlugins;

  lblStatus.Caption := 'Loading MackieControl.ini';
  Application.ProcessMessages;
  CWPluginList.LoadCWPlugins(lblStatus, ProgressBar1);

  lbPlugins.Items.BeginUpdate;
  lbPlugins.Sorted := false;
  for i := 0 to CWPluginList.Count-1 do
  begin
    lbPlugins.AddItem(TCWPlugin(CWPluginList[i]).Name, CWPluginList[i]);
    mplug := MackiePluginList[TCWPlugin(CWPluginList[i]).Name];
    if (mplug <> nil) then
    begin
      for m := 0 to 5 do
        for p := 0 to 31 do
        begin
          mparam := mplug.GetParameterByIndexes(m,p);

          if (mparam <> nil) and (mparam.IsAssigned) then
            mparam.ParameterName := TCWPlugin(CWPluginList[i]).GetParameterName(mparam.ParamNumber);
        end;
    end;
  end;

  lbPlugins.Sorted := true;
  lbPlugins.Items.EndUpdate;
  gbPlugins.Caption := 'Plugins (' + inttostr(lbPlugins.Count) + ')';
  
  lblStatus.Caption := 'Reading Parameters';
  Application.ProcessMessages;

  for i := 0 to MackiePluginList.Count-1 do
    MackiePluginList.ItemsByIndex[i].PopulateMissingParamNamesWithOriginalComment;

  Screen.Cursor := crDefault;
  lblStatus.Caption := 'Ready';

  FPluginsLoaded := true;

  lbPlugins.Refresh;
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
var
  i,m : integer;
begin
  if not(FPluginsLoaded) then
    exit;

  FUpdatingDisplay := true;

  try
    if (FCurrentMackiePlugin = nil) then
    begin
      ebPluginName.Text := '';
      rgPluginType.ItemIndex := 0;
      ebPluginVPots.Text := '32';
      ebPluginFreqBands.Value := 0;

      for i := 0 to 31 do
      begin
        FLabels[i].Color := clSilver;
        FLabels[i].Caption := '';
        FDisplayLabels[i].Color := clSilver;
        FDisplayLabels[i].Caption := '';
      end;
    end
    else
    begin
      ebPluginName.Text := FCurrentMackiePlugin.PluginName;
      rgPluginType.ItemIndex := Ord(FCurrentMackiePlugin.PluginType);
      ebPluginVPots.Text := inttostr(FCurrentMackiePlugin.NumVPots);
      ebPluginFreqBands.Value := FCurrentMackiePlugin.NumFreqBands;

      m := GetCurrentModifier;

      for i := 0 to 31 do
        UpdateDisplayLabel(m, i);
    end;

    ebVPotName.Enabled := false;
    rgVPotType.Enabled := false;
    ebVPotParamName.Enabled := false;
    cbVPotDefault.Enabled := false;
    ebVPotDefault.Enabled := false;
    cbVPotParamIncrement.Enabled := false;
    ebVPotIncrement.Enabled := false;
    cbVPotEQParam.Enabled := false;
    ebVPotDisplayLabel.Enabled := false;

    ebVPotName.Text := '';
    rgVPotType.ItemIndex := 0;
    ebVPotParamName.Text := '';
    cbVPotDefault.Checked := false;
    ebVPotDefault.Text := '';
    cbVPotParamIncrement.Checked := false;
    ebVPotIncrement.Text := '';
    cbVPotEQParam.Text := '';
    ebVPotDisplayLabel.Text := '';


    if (FCurrentMackiePluginParameter <> nil) then
    begin
      ebVPotName.Text := FCurrentMackiePluginParameter.ControlName;
      rgVPotType.ItemIndex := ord(FCurrentMackiePluginParameter.DataType);
      ebVPotParamName.Text := FCurrentMackiePluginParameter.ParameterName;
      cbVPotDefault.Checked := (FCurrentMackiePluginParameter.HasDefaultValue);

      if FCurrentMackiePluginParameter.HasDefaultValue then
        ebVPotDefault.Text := FloatToStr(FCurrentMackiePluginParameter.DefaultValue)
      else
        ebVPotDefault.Text := '';

      cbVPotParamIncrement.Checked := (FCurrentMackiePluginParameter.HasIncrement);

      if FCurrentMackiePluginParameter.HasIncrement then
        ebVPotIncrement.Text := FloatToStr(FCurrentMackiePluginParameter.Increment)
      else
        ebVPotIncrement.Text := '';

      cbVPotEQParam.Text := FCurrentMackiePluginParameter.EQVPot;
      ebVPotDisplayLabel.Text := FCurrentMackiePluginParameter.DisplayLabel;

      if FCurrentMackiePluginParameter.IsAssigned then
      begin
        ebVPotName.Enabled := true;
        rgVPotType.Enabled := true;
        ebVPotParamName.Enabled := true;
        cbVPotDefault.Enabled := true;
        ebVPotDefault.Enabled := true;
        cbVPotParamIncrement.Enabled := true;
        ebVPotIncrement.Enabled := true;
        cbVPotEQParam.Enabled := (rgPluginType.ItemIndex = 1);
        ebVPotDisplayLabel.Enabled := true;
      end;
    end;

    lbParameters.Repaint;
    lbPlugins.Repaint;

    UpdateKnobImages();
  finally
    FUpdatingDisplay := false;
  end;
end;

procedure TForm1.rbModifierClick(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  FCurrentMackiePluginParameter := nil;
  UpdateDisplay;
end;

procedure TForm1.LoadBitmaps;
var
  i : integer;
begin
  for i := 1 to 31 do
      FKnobImages[i].Picture.Assign(FKnobImages[0].Picture);
end;

procedure TForm1.imgTrashCanDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  m, p : integer;
  okToContinue : boolean;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  p := -1;
  okToContinue := false;

  if (Source is TImage) and (Pos('KnobImage',TImage(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TImage(Source).Tag - 1;
    okToContinue := true;
  end;

  if (Source is TStaticText) and (Pos('KnobText',TStaticText(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TStaticText(Source).Tag - 1;
    okToContinue := true;
  end;

  if okToContinue then
  begin
    m := GetCurrentModifier;

    FCurrentMackiePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p);

    if (FCurrentMackiePluginParameter <> nil) then
      Accept := (FCurrentMackiePluginParameter.IsAssigned)
    else
      Accept := false;
  end
  else if (Source = lbPlugins) and (lbPlugins.ItemIndex >= 0) then
    Accept := true
  else
    Accept := false;
end;
{$POP}

procedure TForm1.KnobImage1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  if ssLeft in Shift then
    TControl(Sender).BeginDrag(true);
end;
{$POP}

procedure TForm1.KnobImageClick(Sender: TObject);
var
  m, p : integer;
  okToContinue : boolean;
begin
  CopyImageFromImageList(IDX_BLANK_KNOB_IMAGE, VPotKnobImage);

  if not(FPluginsLoaded) then
    exit;

  btnKeyFocus.SetFocus;

  p := -1;
  okToContinue := false;

  if (Sender is TImage) and (Pos('KnobImage',TImage(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TImage(Sender).Tag - 1;
    okToContinue := true;
  end;

  if (Sender is TStaticText) and (Pos('KnobText',TStaticText(Sender).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TStaticText(Sender).Tag - 1;
    okToContinue := true;
  end;

  if okToContinue then
  begin
    m := GetCurrentModifier;
    FCurrentParameterKnobIdx := p;
    FCurrentMackiePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p);

    UpdateDisplay;
  end
end;

procedure TForm1.rgVPotTypeClick(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  if (FUpdatingDisplay = false) and (FCurrentMackiePlugin <> nil) and (FCurrentMackiePluginParameter <> nil) then
  begin
    FCurrentMackiePluginParameter.DataType := TMackiePluginParameterDataType(ord(rgVPotType.ItemIndex));
    UpdateKnobImages;
  end;
end;

procedure TForm1.ebVPotDefaultChange(Sender: TObject);
var
  v : double;
  err : integer;
begin
  if not(FPluginsLoaded) then
    exit;

  if (FUpdatingDisplay = false) and (FCurrentMackiePlugin <> nil) and (FCurrentMackiePluginParameter <> nil) then
  begin
    if (cbVPotDefault.Checked) then
    begin
      FCurrentMackiePluginParameter.HasDefaultValue := true;
      Val(StringReplace(ebVPotDefault.Text,' ','',[rfReplaceAll]),v,err);

      if (err = 0) then
        FCurrentMackiePluginParameter.DefaultValue := v;
    end
    else
      FCurrentMackiePluginParameter.HasDefaultValue := false;
  end;
end;

procedure TForm1.ebVPotIncrementChange(Sender: TObject);
var
  v : double;
  err : integer;
begin
  if not(FPluginsLoaded) then
    exit;

  if (FUpdatingDisplay = false) and (FCurrentMackiePlugin <> nil) and (FCurrentMackiePluginParameter <> nil) then
  begin
    if (cbVPotParamIncrement.Checked) then
    begin
      FCurrentMackiePluginParameter.HasIncrement := true;
      Val(StringReplace(ebVPotIncrement.Text,' ','',[rfReplaceAll]),v,err);
      if (err = 0) then
        FCurrentMackiePluginParameter.Increment := v;
    end
    else
      FCurrentMackiePluginParameter.HasIncrement := false;
  end;
end;

procedure TForm1.ebPluginVPotsChange(Sender: TObject);
var
  numPots : integer;
begin
  if not(FPluginsLoaded) then
    exit;

  if (FCurrentMackiePlugin <> nil) then
  begin
    try
      numPots := ebPluginVPots.Value;
    except
      on EConvertError do exit;
    end;

    FCurrentMackiePlugin.NumVPots := numPots;
  end;
end;

procedure TForm1.imgTrashCanDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  m, p : integer;
  i : integer;
  okToContinue : boolean;
  {$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  p := -1;
  okToContinue := false;

  if (Source is TImage) and (Pos('KnobImage',TImage(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TImage(Source).Tag - 1;
    okToContinue := true;
  end;

  if (Source is TStaticText) and (Pos('KnobText',TStaticText(Source).Name) > 0) and (FCurrentMackiePlugin <> nil) then
  begin
    p := TStaticText(Source).Tag - 1;
    okToContinue := true;
  end;

  if okToContinue then
  begin
    m := GetCurrentModifier;

    FCurrentMackiePluginParameter := FCurrentMackiePlugin.GetParameterByIndexes(m,p);
    FCurrentMackiePluginParameter.IsAssigned := false;

    if (FCurrentMackiePlugin.AssignedCount = 0) then
    begin
      MackiePluginList.DeletePlugin(FCurrentMackiePlugin);
      FCurrentMackiePlugin := nil;
      FCurrentMackiePluginParameter := nil;
    end;

    UpdateDisplay;
  end
  else if (Source = lbPlugins) and (lbPlugins.ItemIndex >= 0) then
  begin
    i := (MackiePluginList.IndexOfName(lbPlugins.Items[lbPlugins.ItemIndex]));
    if (i >= 0) then
    begin
      MackiePluginList.Delete(i);
      FCurrentMackiePlugin := nil;
      FCurrentMackiePluginParameter := nil;
      UpdateDisplay;
    end;
  end;
end;
{$POP}

procedure TForm1.lbPluginsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  if not(FPluginsLoaded) then
    exit;

  if ssLeft in Shift then
    lbPlugins.BeginDrag(true);
end;
{$POP}

procedure TForm1.WmAfterShow(var Msg: TMessage);
{$PUSH}{$WARN 5024 OFF} // Turn off unused parameters hints - event handlers need these parameters regardless
begin
  Application.ProcessMessages;

  if not(FPluginsLoaded) then LoadDataFiles;
end;
{$POP}

procedure TForm1.btnSaveClick(Sender: TObject);
var
  sl,sl2 : TStringList;
  saveAs : boolean;
  saveFailure : boolean;
  f1,f2 : string;
  i : integer;

  function GetNumberSuffix(i : integer) : string;
  begin
    if i < 10 then
      result := '.00' + inttostr(i)
    else if i < 100 then
      result := '.0' + inttostr(i)
    else
      result := '.' + inttostr(i);
  end;

begin
  if not(FPluginsLoaded) then
    exit;

  txtDebug.Clear;

  saveAs := false;
  saveFailure := false;
  sl := TStringList.Create;
  try
    sl.Text := MackiePluginList.GenerateIni;
    try
      f1 := GetMackieIniFilename;
      txtDebug.Lines.Add('MackieIniFilename is ' + f1);

      i := 1;
      f2 := f1 + GetNumberSuffix(i);
      while FileExists(f2) do
      begin
        i := i + 1;
        f2 := f1 + GetNumberSuffix(i);
      end;

      txtDebug.Lines.Add('Copying MackieIni to ' + f2);

      // attempt to copy contents of f2 to f2
      sl2 := TStringList.Create;
      try
        try
          sl2.LoadFromFile(f1);
          sl2.SaveToFile(f2);
        except
        end;
      finally
        FreeAndNil(sl2);
      end;
      Sleep(1000); // nasty - but gives time for file to flush to disk.

      if FileExists(f2) then
      begin
        txtDebug.Lines.Add(f2 + ' exists');

        if (MessageDlg('Overwrite MackieControl.ini ?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
          sl.SaveToFile(f1)
        else
          saveAs := true;
      end
      else
      begin
        // couldn't save, most likely due to permissions
        txtDebug.Lines.Add(f2 + ' does not exist');
        saveAs := true;
        saveFailure := true;
      end;
    except
      on e:Exception do
      begin
        // couldn't save due to some other reason
        txtDebug.Lines.Add('Error: ' + e.ClassName + ' : ' + e.Message);
        saveAs := true;
        saveFailure := true;
      end;
    end;

    if saveAs then
    begin
      if saveFailure then
      begin
        // Couldn't save to surfaces directory (usually due to permissions), so save in My Documents
        SaveDialog1.InitialDir := TFolderHelper.GetMyDocumentsFolder;
        SaveDialog1.Filename := TFolderHelper.GetMyDocumentsFolder + '\MackieControl.ini';
      end
      else
      begin
        SaveDialog1.InitialDir := GetSurfacesDirectory;
        SaveDialog1.Filename := GetSurfacesDirectory + '\MackieControl.ini';
      end;

      txtDebug.Lines.Add('SaveDialog Dir is: ' + SaveDialog1.Filename);

      if (SaveDialog1.Execute) then
      begin
        if FileExists(SaveDialog1.Filename) then
        begin
          if (MessageDlg('File exists - overwrite ?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
          begin
            sl.SaveToFile(SaveDialog1.Filename);
            txtDebug.Lines.Add('Saving to : ' + SaveDialog1.Filename);
          end;
        end
        else
        begin
          sl.SaveToFile(SaveDialog1.Filename);
          txtDebug.Lines.Add('Saving to : ' + SaveDialog1.Filename);
        end;
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

function TForm1.GetCurrentModifier: integer;
var
  m : integer;
begin
  m := 0;
  if (rbModifierM1.Checked) then
    m := 1
  else if (rbModifierM2.Checked) then
    m := 2
  else if (rbModifierM3.Checked) then
    m := 3
  else if (rbModifierM4.Checked) then
    m := 4;

  result := m;
end;

procedure TForm1.CopyImageFromImageList(imgIdx: integer; var img: TImage);
begin
  KnobImageList.GetBitmap(imgIdx, img.Picture.Bitmap);
end;

procedure TForm1.UpdateKnobImages;
var
  i : integer;
  m : integer;
  param : TMackiePluginParameter;
begin
  if not(FPluginsLoaded) then
    exit;

  m := GetCurrentModifier;

  for i := 0 to 31 do
  begin
    if (FCurrentMackiePlugin <> nil) then
    begin
      param := FCurrentMackiePlugin.GetParameterByIndexes(m, i);
      if (param.IsAssigned) then
        CopyImageFromImageList(ord(param.DataType), FKnobImages[i])
      else
        CopyImageFromImageList(IDX_BLANK_KNOB_IMAGE, FKnobImages[i]);
    end
    else
      CopyImageFromImageList(IDX_BLANK_KNOB_IMAGE, FKnobImages[i]);
  end;

  if (FCurrentMackiePluginParameter <> nil) and (FCurrentMackiePluginParameter.IsAssigned) then
    CopyImageFromImageList(ord(FCurrentMackiePluginParameter.DataType), VPotKnobImage)
  else
    CopyImageFromImageList(IDX_BLANK_KNOB_IMAGE, VPotKnobImage);
end;

procedure TForm1.UpdateDisplayLabel(modifier, index: integer);
var
  param : TMackiePluginParameter;

  function UpperFirstChar(aStr : string) : string;
  begin
    if aStr<>'' then
      aStr[1] := UpCase(aStr[1]);

    result := aStr;
  end;

begin
  if not(FPluginsLoaded) then
    exit;

  if (FCurrentMackiePlugin <> nil) then
  begin
    param := FCurrentMackiePlugin.GetParameterByIndexes(modifier, index);

    if (param.IsAssigned) then
    begin
      FLabels[index].Color := $00F9E3B7;
      FLabels[index].Caption := param.ParameterName;
      FDisplayLabels[index].Color := $00F4C86C;
      FDisplayLabels[index].Font.Color := clBlack;
      if (param.DisplayLabel <> '') then
        FDisplayLabels[index].Caption := trim(param.DisplayLabel)
      else
        FDisplayLabels[index].Caption := TStringCruncher.CrunchString(param.ParameterName,6);
      FKnobImages[index].Hint:= UpperFirstChar(MACKIE_PARAMETER_DATA_TYPES[param.DataType]);
    end
    else
    begin
      FLabels[index].Color := clSilver;
      FLabels[index].Caption := '';
      FDisplayLabels[index].Caption := '';
      FDisplayLabels[index].Color := clSilver;
      FKnobImages[index].Hint:= '';
    end;
  end
  else
  begin
    FLabels[index].Color := clSilver;
    FLabels[index].Caption := '';
    FDisplayLabels[index].Caption := '';
    FDisplayLabels[index].Color := clSilver;
    FKnobImages[index].Hint:= '';
  end;
end;

procedure TForm1.btnReloadClick(Sender: TObject);
begin
  if (MessageDlg('Discard any edits and reload plugins ?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
  begin
    lbCurrentPluginName.Caption := '';
    lbPlugins.ItemIndex := -1;
    cbPluginsChange(Self);
    Application.ProcessMessages;
    LoadDataFiles;
  end;
end;

procedure TForm1.cbVPotEQParamChange(Sender: TObject);
begin
  if not(FPluginsLoaded) then
    exit;

  if (FUpdatingDisplay = false) and (FCurrentMackiePlugin <> nil) and (FCurrentMackiePluginParameter <> nil) then
    FCurrentMackiePluginParameter.EQVPot := cbVPotEQParam.Text;
end;

end.
