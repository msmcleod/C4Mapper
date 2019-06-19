unit XMLClass;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  classes,sysutils,XMLDef;

{ to do: add XML-SPACE=PRESERVE
  Performance optimization : store tag values in list , namespace support
  ENCODING
  URL's instead of filenames
  Illegal characters
  Check against XML tests.}

type
  TokenType = (ttError,ttDeclaration,ttDTD,ttElement,ttEmptyElement,ttEndOfElement,
               ttPCData,ttCData,ttComment,ttPI,ttWhitespace);

  TXMLParserOnParseProgress = procedure (Sender : TObject; CurrentIndex, FileSize : integer) of object; 
  XMLParseException = class(Exception);

  TXMLSourceType = (xsStream,xsFile);

  pCharFile = ^TCharFile;
  TCharFile = file of char;
  pTextFile = ^TextFile;

  TXMLParser = class(TComponent)
  private
    FXMLDocument : TXMLDocument;
    FIncludeWhitespaces : boolean;
    FLastCh : char;
    FSaveLastChar : boolean;
    FFile : PCharFile;
    FOutFile : PTextFile;
    FStream : TStream;
    FLevel : integer;
    FOutStr : string;
    FCharCount : integer;
    FFileSize : integer;
    FOnParseProgress : TXMLParserOnParseProgress;
  protected
    function FileGetCh : char;
    function StreamGetCh : char;
    function ElementTag (const instr : string) : string;
    procedure ProcessAttributes(var CurrentObject : TXMLElement; instr : string);
    function FirstChar(astring : string) : char;
    function NormaliseSpaces (instr : string) : string;
    function GetToken (var CurrentObject : TXMLElement; SourceType : TXMLSourceType) : TokenType;
    procedure SetIncludeWhitespaces (AValue : Boolean);
    function GetXMLDocument : TXMLDocument;
    procedure WriteXML(AObj : TXMLElement;DestType : TXMLSourceType; Pretty : boolean);
  public
    procedure Clear;
    function ParseFromFile (AFileName : string) : integer;
    function ParseFromStream (AStream : TStream) : integer;
    function WriteToFile (AFileName : string; Pretty : boolean) : integer;
    function WriteToStream (AStream : TStream; Pretty : boolean) : integer;
    procedure SetFileSize(AValue : integer);

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property XMLDocument : TXMLDocument read GetXMLDocument;
    property IncludeWhitespaces : boolean read FIncludeWhitespaces write SetIncludeWhitespaces;
    property OnParseProgress : TXMLParserOnParseProgress read FOnParseProgress write FOnParseProgress;
  end;

  procedure Register;

implementation

const
  WhiteSpace : set of char = [' ',#9,#10,#13];
  spaces='                                                                      '+
         '                                                                      '+
         '                                                                      ';

  nlcr = #13+#10;

procedure Register;
begin
  RegisterComponents('XML', [TXMLParser]);
end;

constructor TXMLParser.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FXMLDocument := nil;
  FIncludeWhitespaces := false;
  FLastCh := #0;
  FSaveLastChar := false;
  FFile := nil;
  FStream := nil;
  FOnParseProgress := nil;
  FFileSize := -1;
  FCharCount := 0;
end;

destructor TXMLParser.Destroy;
begin
  if FXMLDocument<>nil then
    FXMLDocument.Free;
  FFile := nil;
  FStream := nil;
  inherited Destroy;
end;

function TXMLParser.FileGetCh : char;
var
  newch : char;
begin
  { we could decrement the 'position' property instead of using save_last_char
  but this way is more suitable for text files. }
  if FSaveLastChar then
  begin
    result := FLastCh;
    FSaveLastChar := false;
    exit;
  end;
  { read character from file }
  if not(eof(FFile^)) then
    Read(FFile^,newch)
  else
    raise XMLParseException.Create('End of file.');
  FLastCh := newch;
  result := newch;
end;

function TXMLParser.StreamGetCh : char;
var
  newch : char;
begin
  newch := #0;
  { we could decrement the 'position' property instead of using save_last_char
  but this way is more suitable for text files. }
  if FSaveLastChar then
  begin
    result := FLastCh;
    FSaveLastChar := false;
    exit;
  end;
  { read character from stream }
  FStream.Read(newch,1);
  FLastCh := newch;
  result := newch;
end;

function TXMLParser.ElementTag (const instr : string) : string;
var
  tempstr : string;
  loop : integer;
begin
  tempstr := '';
  for loop := 1 to length(instr) do
  begin
    if not(instr[loop] in ['/','?','!','>','<']) then
    begin
      if instr[loop] in WhiteSpace then
        break // end of element name
      else
        tempstr := tempstr+instr[loop];
    end;
  end;
  result := tempstr;
end;

procedure TXMLParser.ProcessAttributes(var CurrentObject : TXMLElement; instr : string);
var
  myTag,myValue : string;

  function GetAttribute (var l_instr,Tag,Value : string) : boolean;
  var
    l_count : integer;
  begin
    Tag := '';
    Value := '';
    result := false;
    l_count := 0;
    // extract tag
    while l_count<length(l_instr) do
    begin
      inc(l_count);
      if l_instr[l_count]<>'=' then
      begin
        if not(l_instr[l_count] in WhiteSpace) then
          Tag := Tag+l_instr[l_count];
      end
      else
        break;
    end;
    if l_count=length(l_instr) then
      exit;
    // get first quote
    while l_count<length(l_instr) do
    begin
      inc(l_count);
      if l_instr[l_count]='"' then
        break;
    end;
    if l_count=length(l_instr) then
      exit;
    // extract value
    while l_count<length(l_instr) do
    begin
      inc(l_count);
      if l_instr[l_count]='"' then
        break
      else
        Value := Value + l_instr[l_count];
    end;
    if l_count=length(l_instr) then
      exit; // we should at least have a '>' at the end.
    l_instr := copy(l_instr,l_count+1,length(l_instr)-l_count);
    result := true;
  end;

begin // Process Attributes
  { take out element tag... }
  while not(instr[1] in WhiteSpace) do
    if length(instr)>1 then
      instr := copy(instr,2,length(instr)-1)
    else
      break;
  { add each attribute }
  while GetAttribute(instr,myTag,myValue) do
    CurrentObject.NewAttribute(myTag,myValue);
end;

function TXMLParser.FirstChar(astring : string) : char;
var
  loop : integer;
begin
  result := #0;
  for loop := 1 to length(astring) do
    if (not(astring[loop] in whitespace)) then
    begin
      result := astring[loop];
      exit;
    end;
end;

function TXMLParser.NormaliseSpaces (instr : string) : string;
var
  loop : integer;
  no_duplicate_whitespaces : boolean;
begin
  // remove preceding white-spaces
  while (instr[1] in whitespace) do
    instr := copy (instr,2,length(instr)-1);
  // replace trailing white-spaces with one space
  if (instr[length(instr)] in whitespace) then
  begin
    while (instr[length(instr)] in whitespace) do
      instr := copy (instr,1,length(instr)-1);
    instr := instr+' ';
  end;
  // replace any tabs,newlines etc.. with spaces
  for loop := 1 to length(instr) do
    if (instr[loop] in whitespace) then
      instr[loop] := ' ';
  // replace duplicate whitespaces with one space.
  repeat
    no_duplicate_whitespaces := true;
    for loop := 1 to length(instr)-1 do
    begin
      if (instr[loop] in whitespace) and (instr[loop+1] in whitespace) then
      begin
        instr := copy(instr,1,loop-1)+copy(instr,loop+1,length(instr)-loop);
        no_duplicate_whitespaces := false;
      end;
    end;
  until no_duplicate_whitespaces;
  result := instr;
end;

function TXMLParser.GetToken (var CurrentObject : TXMLElement; SourceType : TXMLSourceType) : TokenType;
var
  instr : string;
  ch : char;
  tempobj : TXMLElement;
begin
  if Assigned(FOnParseProgress) then
    FOnParseProgress(Self, FCharCount, FFileSize);

  result := ttError;
  try
    instr := '';
    repeat
      if SourceType=xsStream then
        ch := StreamGetCh
      else
        ch := FileGetCh;
      FCharCount := FCharCount + 1;
      instr := instr+ch;
    until (ch='<');
    if length(instr)>1 then
    begin
      FSaveLastChar := true; // we want to keep the '<' for next element...
      if firstchar(instr)<>'<' then
        CurrentObject.AddChild(TXMLPCData.New(CurrentObject,NormaliseSpaces(copy(instr,1,length(instr)-1))))
      else
      begin
        result := ttWhitespace;
        if IncludeWhitespaces then
          CurrentObject.AddChild(TXMLWhitespace.New(CurrentObject,copy(instr,1,length(instr)-1)))
        else
          exit;
      end;
    end
    else
    begin
      if instr='<' then
      begin
        repeat
          if SourceType=xsStream then
            ch := StreamGetCh
          else
            ch := FileGetCh;
          FCharCount := FCharCount + 1;
          instr := instr+ch;
        until (ch='>');
        if pos('<?XML',uppercase(instr))>0 then
        begin
          result := ttDeclaration;
          // Declaration
          if CurrentObject is TXMLDocument then
          begin
            ProcessAttributes(CurrentObject,instr);
            with (CurrentObject as TXMLDocument) do
            begin
              FXMLType := ElementTag(instr);
              GetAttribute('VERSION',FXMLVersion);
              if GetAttribute('STANDALONE',instr)=XML_OK then
                FStandalone := (instr='YES')
              else
                FStandalone := true; // default to true here...
              GetAttribute('ENCODING',FEncoding);
            end;
          end
          else
            raise XMLParseException.Create('Unexpected declaration found');
          exit;
        end;
        if instr[2]='?' then
        begin
          if pos('[',instr)>0 then
          while pos(']?>',instr)>0 do
          begin
            if SourceType=xsStream then
              ch := StreamGetCh
            else
              ch := FileGetCh;
            FCharCount := FCharCount + 1;
            instr := instr+ch;
          end;
          result := ttPI;
          CurrentObject.AddChild(TXMLCData.New(CurrentObject,copy(instr,10,length(instr)-12))); // Add CDATA
          exit;
        end;
        if pos('<![CDATA[',instr)=1 then
        begin
          // check we've reached the end of the CDATA statement....
          while pos(']]>',instr)=0 do
          begin
            if SourceType=xsStream then
              ch := StreamGetCh
            else
              ch := FileGetCh;
            FCharCount := FCharCount + 1;
            instr := instr+ch;
          end;
          if length(instr)>12 then
          begin
            result := ttCData;
            CurrentObject.AddChild(TXMLCData.New(CurrentObject,copy(instr,10,length(instr)-12))); // Add CDATA
            exit;
          end
          else
            raise XMLParseException.Create('Invalid CDATA declaration');
        end;
        if instr[2]='!' then
        begin
          if length(instr)>6 then
          begin
            result := ttComment;
            CurrentObject.AddChild(TXMLComment.New(CurrentObject,copy(instr,3,length(instr)-3))); // Add Comment
            exit;
          end
          else
            raise XMLParseException.Create('Invalid comment declaration');
        end;
        if instr[2]='/' then // end of element
        begin
          result := ttEndOfElement;
          if (ElementTag(instr)=CurrentObject.FTag) or (ElementTag(instr)='') then
          begin
            CurrentObject := TXMLElement(CurrentObject.Parent);
            exit;
          end
          else
            raise XMLParseException.Create ('Unexpected end of element:'+nlcr+
                 'Expected: '+CurrentObject.FTag+nlcr+'Found: '+ElementTag(instr));
        end;
        if instr[length(instr)-1]='/' then // empty element
        begin
          TempObj := CurrentObject.NewChild(ElementTag(instr));
          ProcessAttributes(tempobj,instr);
          result := ttEmptyElement;
          exit;
        end;
        // must be element type...
        result := ttElement;
        CurrentObject := CurrentObject.NewChild(ElementTag(instr)); // Add Element
        ProcessAttributes(CurrentObject,instr);
      end
      else
        raise XMLParseException.Create ('No element found.');
    end;
  except
    on E:Exception do
     raise; // re-raise exception for caller
  end;
end;

procedure TXMLParser.Clear;
begin
  if FXMLDocument<>nil then
    FXMLDocument.Free;
  FXMLDocument := nil;
end;

function TXMLParser.ParseFromFile (AFilename : string) : integer;
var
  ior : integer;
  myfile : TCharFile;
  myobj : TXMLElement;
begin
  result := 0;
  assignfile (myfile,AFilename);
  FCharCount := 0;
  {$I-}
  reset(myfile);
  FFileSize := FileSize(myfile);
  ior := ioresult;
  {$I+}
  if ior<>0 then
  begin
    result := ior;
    exit;
  end;
  try
    if FXMLDocument=nil then
      FXMLDocument := TXMLDocument.Create(nil);
    FFile := PCharFile(@myfile);
    myobj := FXMLDocument;
    repeat
      GetToken(myobj,xsFile);
    until myobj<>FXMLDocument;
    repeat
      GetToken(myobj,xsFile);
    until myobj=FXMLDocument;
  finally
    CloseFile(myfile);
    FFile := nil;
  end;
end;

function TXMLParser.ParseFromStream (AStream : TStream) : integer;
var
  myobj : TXMLElement;
begin
  result := 0;
  FFileSize := AStream.Size;
  try
    if FXMLDocument=nil then
      FXMLDocument := TXMLDocument.Create(nil);
    FStream := AStream;
    myobj := FXMLDocument;
    repeat
      GetToken(myobj,xsStream);
    until myobj<>FXMLDocument;
    repeat
      GetToken(myobj,xsStream);
    until myobj=FXMLDocument;
  finally
    FStream := nil;
  end;
end;

function TXMLParser.WriteToFile (AFilename : string; Pretty : boolean) : integer;
var
  ior : integer;
  myfile : TextFile;
begin
  result := 0;
  AssignFile (myfile,AFilename);
  {$I-}
  rewrite(myfile);
  ior := ioresult;
  {$I+}
  if ior<>0 then
  begin
    result := ior;
    exit;
  end;
  FLevel := -2;
  try
    FOutFile := pTextFile(@myfile);
    WriteXML(FXMLDocument,xsFile,Pretty);
  finally
    CloseFile(myfile);
    FOutFile := nil;
  end;
end;

function TXMLParser.WriteToStream (AStream : TStream; Pretty : boolean) : integer;
begin
  FLevel := -2;
  try
    FOutStr := '';
    FStream := AStream;
    WriteXML(FXMLDocument,xsStream,Pretty);
    if FOutStr<>'' then
    begin
      FOutStr := FOutStr+nlcr;
      FStream.Write(FOutStr,length(FOutStr));
    end;
  finally
    result := 0;
    FStream := nil;
    FOutStr := '';
  end;
end;

procedure TXMLParser.SetIncludeWhitespaces (AValue : Boolean);
begin
  FIncludeWhitespaces := AValue;
end;

function TXMLParser.GetXMLDocument : TXMLDocument;
begin
  result := FXMLDocument;
end;

procedure TXMLParser.WriteXML(AObj : TXMLElement;DestType : TXMLSourceType; Pretty : boolean);
var
  loop : integer;
  idstr : string;
  obj : TXMLObject;
  AFile : PTextFile;
begin
  AFile := FOutFile;
  inc(FLevel);
  idstr := '';
  if (Pretty or not(IncludeWhitespaces)) and (FLevel>0) then
    idstr := copy(spaces,1,FLevel*2);

  if DestType=xsFile then
  begin
    if Pretty or (not(IncludeWhitespaces)) then
      writeln(AFile^);
    if Pretty then
      write(AFile^,idstr+AObj.GetAsXML)
    else
      write(AFile^,AObj.GetAsXML);
  end
  else
  begin
    if Pretty or (not(IncludeWhitespaces)) then
    begin
      FOutStr := FOutStr+nlcr;
      FStream.Write(FOutStr,length(FOutStr));
      FOutStr := '';
    end;
    if Pretty then
      FOutStr := FOutStr+idstr+AObj.GetAsXML
    else
      FOutStr := FOutStr+AObj.GetAsXML;
  end;

  obj := AObj;
  if (AObj.ChildCount>0) then
  begin
    for loop := 0 to AObj.ChildCount-1 do
    begin
      obj := AObj.ChildItem(loop);
      if (AObj.ChildItem(loop) is TXMLElement) then
        WriteXML(AObj.ChildItem(loop) as TXMLElement,DestType,Pretty)
      else
      begin
        if obj is TXMLWhiteSpace then
        begin
          if IncludeWhitespaces and not(pretty) then
          begin
            if DestType=xsFile then
              write(AFile^,AObj.ChildItem(loop).GetAsXML)
            else
              FOutStr := FOutStr+AObj.ChildItem(loop).GetAsXML;
          end;
        end
        else
        begin
          if obj is TXMLPCData then
          begin
            if DestType=xsFile then
              write(AFile^,AObj.ChildItem(loop).GetAsXML)
            else
              FOutStr := FOutStr+AObj.ChildItem(loop).GetAsXML;
          end
          else
          begin
            if DestType=xsFile then
            begin
              if Pretty or (not(IncludeWhitespaces)) then
                writeln(AFile^);
              if Pretty then
                write(AFile^,idstr+'  '+AObj.ChildItem(loop).GetAsXML)
              else
                write(AFile^,AObj.ChildItem(loop).GetAsXML);
            end
            else
            begin
              if Pretty or (not(IncludeWhitespaces)) then
              begin
                FOutStr := FOutStr+nlcr;
                FStream.Write(FOutStr,length(FOutStr));
                FOutStr := '';
              end;
              if Pretty then
                FOutStr := FOutStr+idstr+AObj.ChildItem(loop).GetAsXML
              else
                FOutStr := FOutStr+AObj.ChildItem(loop).GetAsXML;
            end;
          end;
        end;
      end;
    end;
  end;

  if (not(AObj is TXMLDocument)) and (not(AObj.Empty)) then
  begin
    if (obj is TXMLPCData) and (obj.Parent=AObj) then
    begin
      if DestType=xsFile then
        write(AFile^,'</'+AObj.FTag+'>')
      else
        FOutStr := FOutStr+'</'+AObj.FTag+'>';
    end
    else
    begin
      if DestType=xsFile then
      begin
        if Pretty or (not(IncludeWhitespaces)) then
          writeln(AFile^);
        if Pretty then
          write(AFile^,idstr+'</'+AObj.FTag+'>')
        else
          write(AFile^,'</'+AObj.FTag+'>');
      end
      else
      begin
        if Pretty or (not(IncludeWhitespaces)) then
        begin
          FOutStr := FOutStr+nlcr;
          FStream.Write(FOutStr,length(FOutStr));
          FOutStr := '';
        end;
        if Pretty then
          FOutStr := FOutStr+idstr+'</'+AObj.FTag+'>'
        else
          FOutStr := FOutStr+'</'+AObj.FTag+'>';
      end;
    end;
  end;
  dec(FLevel);
end;

procedure TXMLParser.SetFileSize(AValue: integer);
begin
  FFileSize := AValue;
end;

end.
