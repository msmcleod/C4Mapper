unit XMLDef;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes;

const
  etXMLObject      = 1;
  etXMLElement     = 2;
  etXMLDocument    = 3;
  etXMLAttribute   = 4;
  etXMLPCData      = 5;
  etXMLCData       = 6;
  etXMLComment     = 7;
  etXMLWhitespace  = 8;
  etXMLPI          = 9;

type
  { Generic Error Type }
  TXMLError = (XML_OK,XML_DUPLICATE_ATTRIBUTE,XML_UNKNOWN_ATTRIBUTE, XML_INVALID_FILE, XML_INVALID_INDEX);

  { TList of XML objects }
  TXMLObjectList = class(TList);

  { Base class for all XML objects }
  TXMLObject = class(TObject)
  private
    FParent : TXMLObject;
  public
    function GetAsText : string; virtual; // content without markups
    function GetAsXML : string; virtual;  // content with markups
    constructor Create(AParent : TXMLObject);
    property Parent : TXMLObject read FParent;
  end;

  { Element class }
  TXMLElement = class(TXMLObject)
  private
    FChildren : TXMLObjectList;  // List children, pcdata etc..
    FAttributes : TXMLObjectList;// List of attributes.
  protected
    function GetAttributeCount : integer;
    function GetChildCount : integer;
    function GetEmpty : boolean;
  public
    FTag : string;               // Element Tag
    function GetAsXML : string; override;
    constructor New(AParent : TXMLObject; ATag : string);
    constructor Create(AParent : TXMLObject);
    destructor Destroy; override;
    function NewAttribute(const AName,AValue : string) : TXMLError;
    function SetAttribute(const AName,AValue : string) : TXMLError;
    function GetAttribute(const AName : string; var AValue : string) : TXMLError;
    function RemoveAttribute(const AName : string) : TXMLError;
    function NewChild(const ATag : string) : TXMLElement;
    procedure AddChild(AObject : TXMLObject);
    function RemoveChild(AElement : TXMLElement) : TXMLError;
    property AttributeCount : integer read GetAttributeCount;
    function AttributeName(AItem : integer) : string;
    property ChildCount : integer read GetChildCount;
    function ChildItem(AItem : integer) : TXMLObject;
    property Empty : boolean read GetEmpty;
  end;

  { Element's Attribute }
  TXMLAttribute = class(TXMLObject)
    FTag : string;
    FValue : string;
    function GetAsXML : string; override;
    constructor New(AParent : TXMLObject;const ATag,AValue : string);
  end;

  { Base class for PC Data, CData, comments etc.. }
  TXMLText = class(TXMLObject)
    FValue : string;
    constructor New(AParent : TXMLObject;const AValue : string);
  end;

  { PC Data class }
  TXMLPCData = class(TXMLText)
    function GetAsXML : string; override;
  end;

  { CData class }
  TXMLCData = class(TXMLText)
    function GetAsXML : string; override;
  end;

  { Comments class }
  TXMLComment = class(TXMLText)
    function GetAsXML : string; override;
  end;

  TXMLWhitespace = class(TXMLText)
    function GetAsXML : string; override;
  end;

  TXMLPI = class(TXMLText)
    function GetAsXML : string; override;
  end;

  { Document class. This describes the XML document and maintains a list
  of both XML elements and the DTD rules associated with them.}

  TXMLDocument = class(TXMLElement)
  private
    FDTD : TXMLObjectList;        // if Valid, TList of DTD rules.
  public
    FXMLType : string;            // XML / SGML / HTML - flag error for != XML?
    FXMLVersion : string;         // eg "1.0"
    FStandalone : boolean;        // Valid/Well-formed
    FEncoding : string;           // eg "UTF-8"
    FDocumentType : string;       // if Valid, document DTD type, eg MEMO
    FDocumentDTD : string;        // if Valid, Filename of associated DTD

    function GetAsXML : string; override;
    constructor Create (AParent : TXMLObject);
    destructor Destroy; override;

    property DTD : TXMLObjectList read FDTD;
  end;


implementation

uses
  SysUtils;

// ======================
// TXMLObject
// ======================

constructor TXMLObject.Create(AParent : TXMLObject);
begin
  inherited Create;
  FParent := AParent;
end;

function TXMLObject.GetAsText : string;
begin
  result := '';
end;

function TXMLObject.GetAsXML : string;
begin
  result := '';
end;

// ======================
// TXMLElement
// ======================

constructor TXMLElement.Create(AParent : TXMLObject);
begin
  inherited Create(AParent);
  FChildren := TXMLObjectList.Create;
  FAttributes := TXMLObjectList.Create;
end;

constructor TXMLElement.New(AParent : TXMLObject; ATag : string);
begin
  inherited Create(AParent);
  FTag := ATag;
  FChildren := TXMLObjectList.Create;
  FAttributes := TXMLObjectList.Create;
end;


destructor TXMLElement.Destroy;
begin
  try
    FChildren.Clear;
    FChildren.Free;
    FAttributes.Clear;
    FAttributes.Free;
  finally
    inherited Destroy;
  end;
end;

function TXMLElement.GetEmpty : boolean;
begin
  result := (ChildCount=0);
end;

function TXMLElement.GetAsXML : string;
var
  outstr : string;
  loop : integer;
begin
  outstr := '<'+FTag;
  if AttributeCount>0 then
      outstr := outstr+' ';
  for loop := 0 to AttributeCount-1 do
  begin
    outstr := outstr+(TXMLAttribute(FAttributes.Items[loop]).GetAsXML);
    if loop<(AttributeCount-1) then
      outstr := outstr+' ';
  end;
  if Empty then
    outstr := outstr+'/>'
  else
    outstr := outstr+'>';
  result := outstr;
end;

function TXMLElement.NewAttribute(const AName,AValue : string) : TXMLError;
var
  loop : integer;
begin
  result := XML_OK;
  for loop := 0 to FAttributes.Count-1 do
    if uppercase(TXMLAttribute(FAttributes.Items[loop]).FTag)=uppercase(AName) then
    begin
      result := XML_DUPLICATE_ATTRIBUTE;
      exit;
    end;
  FAttributes.Add(TXMLAttribute.New(Self,AName,AValue));
end;

function TXMLElement.SetAttribute(const AName,AValue : string) : TXMLError;
var
  loop : integer;
begin
  for loop := 0 to FAttributes.Count-1 do
    if (uppercase(TXMLAttribute(FAttributes.Items[loop]).FTag))=uppercase(AName) then
    begin
      TXMLAttribute(FAttributes.Items[loop]).FValue := AValue;
      result := XML_OK;
      exit;
    end;
  result := XML_UNKNOWN_ATTRIBUTE;
end;

function TXMLElement.GetAttribute(const AName : string; var AValue : string) : TXMLError;
var
  loop : integer;
begin
  for loop := 0 to FAttributes.Count-1 do
    if uppercase(TXMLAttribute(FAttributes.Items[loop]).FTag)=uppercase(AName) then
    begin
      AValue := TXMLAttribute(FAttributes.Items[loop]).FValue;
      result := XML_OK;
      exit;
    end;
  result := XML_UNKNOWN_ATTRIBUTE;
end;

function TXMLElement.NewChild(const ATag : string) : TXMLElement;
var
  myElement : TXMLElement;
begin
  myElement := TXMLElement.New(Self,ATag);
  FChildren.Add(myElement);
  result := myElement;
end;

procedure TXMLElement.AddChild(AObject : TXMLObject);
begin
  FChildren.Add(AObject);
end;

function TXMLElement.GetAttributeCount : integer;
begin
  result := FAttributes.Count;
end;

function TXMLElement.GetChildCount : integer;
begin
  result := FChildren.Count;
end;

function TXMLElement.AttributeName(AItem : integer) : string;
begin
  result := '';
  if (AItem>=0) and (AItem<AttributeCount) then
    result := (TXMLAttribute(FAttributes.Items[AItem]).FTag);
end;

function TXMLElement.ChildItem(AItem : integer) : TXMLObject;
begin
  result := nil;
  if (AItem>=0) and (AItem<ChildCount) then
    result := (TXMLObject(FChildren.Items[AItem]));
end;

function TXMLElement.RemoveAttribute(const AName : string) : TXMLError;
var
  loop : integer;
begin
  result := XML_UNKNOWN_ATTRIBUTE;
  for loop := 0 to AttributeCount-1 do
    if uppercase(TXMLAttribute(FAttributes.Items[loop]).FTag)=uppercase(AName) then
    begin
      FAttributes.Remove(FAttributes.Items[loop]);
      result := XML_OK;
      exit;
    end;
end;

function TXMLElement.RemoveChild(AElement : TXMLElement) : TXMLError;
begin
  result := XML_OK;
  FChildren.Remove(pointer(AElement));
end;

// ======================
// TXMLAttribute
// ======================

constructor TXMLAttribute.New(AParent : TXMLObject;const ATag,AValue : string);
begin
  inherited Create(AParent);
  FTag := ATag;
  FValue := AValue;
end;

function TXMLAttribute.GetAsXML : string;
begin
  result := FTag+'="'+FValue+'"';
end;

// ======================
// TXMLPCData
// ======================

function TXMLPCData.GetAsXML : string;
begin
  result := FValue;
end;

// ======================
// TXMLCData
// ======================

function TXMLCData.GetAsXML : string;
begin
  result := '<![CDATA['+FValue+']]>';
end;

// ======================
// TXMLComment
// ======================

function TXMLComment.GetAsXML : string;
begin
  result := '<!'+FValue+'>';
end;

// ======================
// TXMLWhitespace
// ======================

function TXMLWhitespace.GetAsXML : string;
begin
  result := FValue;
end;

// ======================
// TXMLPI
// ======================

function TXMLPI.GetAsXML : string;
begin
  result := '<?'+FValue+'?>';
end;

// ======================
// TXMLText
// ======================

constructor TXMLText.New(AParent : TXMLObject;const AValue : string);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

// ======================
// TXMLDocument
// ======================


constructor TXMLDocument.Create (AParent : TXMLObject);
begin
  inherited Create (AParent);
  FDTD := TXMLObjectList.Create;
end;

destructor TXMLDocument.Destroy;
begin
  try
    FDTD.Clear;
    FDTD.Free;
  finally
    inherited Destroy;
  end;
end;

function TXMLDocument.GetAsXML : string;
var
  outstr : string;
  loop : integer;
begin
  outstr := '<?'+FXMLType;
  if AttributeCount>0 then
     outstr := outstr+' ';
  for loop := 0 to AttributeCount-1 do
  begin
    outstr := outstr+(TXMLAttribute(FAttributes.Items[loop]).GetAsXML);
    if loop<(AttributeCount-1) then
      outstr := outstr+' ';
  end;
  outstr := outstr+'?>';
  result := outstr;
end;

end.
