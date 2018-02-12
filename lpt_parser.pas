unit lpt_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lptypes, lpparser, lpmessages, lpcompiler, lpvartypes;

type
  TDeclaration = class;
  TDeclaration_Type = class;
  TDeclaration_MethodHeader = class;
  TDeclaration_Method = class;

  TDeclarationStack = specialize TLapeStack<TDeclaration>;
  TDeclarationList = specialize TLapeList<TDeclaration>;
  TDeclarationMap = specialize TLapeStringMap<TDeclaration>;

  TLapeTools_Caret = record
    Reached: Boolean;
    Pos: Int32;
  end;

  TLapeTools_Comment = record
    Start: Int32;
    Len: Int32;
  end;

  TLapeTools_LibraryDirective = function(Argument: String): String of object;

  ELapeTools_ParserSetting = (psParseIncludes, psAddMethodUnderType);
  ELapeTools_ParserSettings = set of ELapeTools_ParserSetting;

  TLapeTools_Parser = class(TLapeCompiler)
  protected
    FData: TDeclarationList;
    FStack: TDeclarationStack;
    FMap: TDeclarationMap;
    FComment: TLapeTools_Comment;
    FCaret: TLapeTools_Caret;
    FInMethod: TDeclaration_Method;
    FPaths: TStringList;
    FFilePath: lpString;
    FFileAge: Int32;
    FSettings: ELapeTools_ParserSettings;
    FOnLibraryDirective: TLapeTools_LibraryDirective;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; override;
  public
    property Map: TDeclarationMap read FMap;
    property InMethod: TDeclaration_Method read FInMethod;
    property Data: TDeclarationList read FData;
    property Stack: TDeclarationStack read FStack;
    property Caret: TLapeTools_Caret read FCaret;
    property Comment: TLapeTools_Comment read FComment;
    property FilePath: lpString read FFilePath;
    property FileAge: Int32 read FFileAge write FFileAge;
    property Paths: TStringList read FPaths;
    property Includes: TStringList read FIncludes;
    property Settings: ELapeTools_ParserSettings read FSettings write FSettings;
    property BaseDefines: TStringList read FBaseDefines;
    property OnLibraryDirective: TLapeTools_LibraryDirective read FOnLibraryDirective write FOnLibraryDirective;

    function GetType(Declaration: TDeclaration): TDeclaration_Type;

    function FindFile(AFilePath: lpString): lpString;
    function Find(Name: lpString): TDeclaration;

    function ParseExpression(Expression: lpString; ReturnType: Boolean = False): TDeclaration; overload;
    procedure Parse;

    function Doc: lpString; inline;
    function DocPos: TDocPos; inline;

    function Read(Tokens: Int32; IgnoreTokens: EParserTokenSet): lpString; inline;
    function Next: EParserToken; override;
    function Tok: EParserToken; inline;
    function TokStart: Int32; inline;
    function TokString: lpString; inline;

    function HasDeclaration(Name: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; override;

    constructor Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True; AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True); reintroduce; override;
    constructor Create(ADoc: lpString; AFilePath: lpString; ACaret: Int32); virtual; overload;
    constructor Create(AFilePath: lpString); virtual; overload;
    destructor Destroy; override;
  end;

  TDeclaration = class
  protected
    FParser: TLapeTools_Parser;
    FState: Pointer;
    FTokens: Int32;
    FText: lpString;
    FInScope: Boolean;
    FDocPos: TDocPos;

    function getDocPos: TDocPos; virtual;
    function getText: String; virtual;
  public
    property Text: lpString read getText write FText;
    property DocPos: TDocPos read getDocPos write FDocPos;
    property InScope: Boolean read FInScope;
    property Tokens: Int32 read FTokens write FTokens;

    (* We parse everything on creation. If a exception is raised on creation the
       object is freed. Thankfully this is only called when creation is successful.
       At that point we can safely manage the object and pop the token stack *)
    procedure AfterConstruction; override;

    constructor Create(Parser: TLapeTools_Parser; getState: Boolean); virtual;
    destructor Destroy; override;
  end;

  TDeclaration_Include = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
    function getText: String; override;
  public
    Path: lpString;
  end;

  TDeclaration_Identifier = class(TDeclaration)
  public
    constructor Create(Parser: TLapeTools_Parser); reintroduce;
  end;
  TDeclaration_Identifiers = array of TDeclaration_Identifier;

  TDeclaration_Value = class(TDeclaration)
  public
    constructor Create(Parser: TLapeTools_Parser); reintroduce;
  end;

  TDeclaration_Type_Identity = class of TDeclaration_Type;
  TDeclaration_Types = array of TDeclaration_Type;
  TDeclaration_Type = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
  public
    Name: TDeclaration_Identifier;

    function Parents(Parser: TLapeTools_Parser): TDeclaration_Types;

    class function Identify(Parser: TLapeTools_Parser; VarType: Boolean): TDeclaration_Type_Identity;

    constructor Create(Parser: TLapeTools_Parser); reintroduce; virtual;
  end;

  TDeclaration_Type_Identifier = class(TDeclaration_Type)
  public
    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Record = class(TDeclaration_Type)
  public
    Parent: TDeclaration_Type_Identifier;
    Fields: TDeclarationMap;

    constructor Create(Parser: TLapeTools_Parser); override;
    destructor Destroy; override;
  end;

  TDeclaration_Type_Alias = class(TDeclaration_Type)
  public
    AliasType: TDeclaration_Type_Identifier;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Enum = class(TDeclaration_Type)
  protected
    function getDocPos: TDocPos; override;
  public
    Elements: TDeclarationMap;

    constructor Create(Parser: TLapeTools_Parser); override;
    destructor Destroy; override;
  end;

  TDeclaration_EnumElement = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
  public
    Enum: TDeclaration_Type_Enum;
    Name: TDeclaration_Identifier;
    Value: TDeclaration_Value;

    constructor Create(Parser: TLapeTools_Parser); reintroduce;
  end;

  TDeclaration_Type_Set = class(TDeclaration_Type)
  public
    SetType: TDeclaration_Value;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Method = class(TDeclaration_Type)
  public
    Header: TDeclaration_MethodHeader;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Array = class(TDeclaration_Type)
  public
    ArrayType: TDeclaration_Type;
    Dimensions: Int32;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Copy = class(TDeclaration_Type)
  public
    CopyType: TDeclaration_Type_Identifier;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Type_Pointer = class(TDeclaration_Type)
  public
    PointerType: TDeclaration_Type_Identifier;

    constructor Create(Parser: TLapeTools_Parser); override;
  end;

  TDeclaration_Variable = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
  public
    Name: TDeclaration_Identifier;
    VarType: TDeclaration_Type;
    Value: TDeclaration_Value;

    constructor Create(Parser: TLapeTools_Parser); reintroduce;
  end;

  EParameterModifier = (pmNone, pmVar, pmOut, pmConstRef, pmConst);

  TDeclaration_Parameter = class(TDeclaration_Variable)
  public
    Modifier: EParameterModifier;
    Group: Int32;
  end;

  TDeclaration_Parameters = class(specialize TLapeStringMap<TDeclaration_Parameter>)
  public
    function Get(Index: Int32): TDeclaration_Parameter;
  end;

  TDeclaration_Constant = class(TDeclaration_Variable);
  TDeclaration_Field = class(TDeclaration_Variable);

  TDeclaration_Label = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
  public
    Name: TDeclaration_Identifier;

    constructor Create(Parser: TLapeTools_Parser); reintroduce;
  end;

  EMethodType = (mtProcedure, mtProcedureOfObject, mtFunction, mtFunctionOfObject, mtOperator);
  EMethodDirective = (mdOverload, mdOverride, mdStatic, mdForward, mdExternal);
  EMethodTypeSet = set of EMethodType;
  EMethodDirectives = set of EMethodDirective;

  TDeclaration_MethodHeader = class(TDeclaration)
  public
    Name: TDeclaration_Identifier;
    ObjectName: TDeclaration_Type_Identifier;
    Result: TDeclaration_Type;
    Parameters: TDeclaration_Parameters;
    MethodType: EMethodType;
    Directives: EMethodDirectives;

    constructor Create(Parser: TLapeTools_Parser); reintroduce;
    destructor Destroy; override;
  end;

  TDeclaration_Methods = array of TDeclaration_Method;

  TDeclaration_Method = class(TDeclaration)
  protected
    function getDocPos: TDocPos; override;
  public
    Header: TDeclaration_MethodHeader;
    Parent: TDeclaration_Method;
    Locals: TDeclarationMap;
    Documentation: lpString;

    function Description: lpString;

    constructor Create(Parser: TLapeTools_Parser); reintroduce;
    destructor Destroy; override;
  end;

  TDeclarationMap_Helper = class helper for TDeclarationMap
  public
    procedure ParseVariables(Parser: TLapeTools_Parser);
    procedure ParseConstants(Parser: TLapeTools_Parser);
    procedure ParseTypes(Parser: TLapeTools_Parser);
    procedure ParseLabels(Parser: TLapeTools_Parser);
    function ParseMethod(Parser: TLapeTools_Parser): TDeclaration_Method;

    function GetMethods(Name: lpString): TDeclaration_Methods;
    function GetRecord(Name: lpString): TDeclaration_Type_Record;
    function GetType(Name: lpString): TDeclaration_Type; inline;
    function Get(Index: Int32): TDeclaration; inline;
  end;

  // Helper to avoid a lot of duplicate code
  TVariableParser = class
  public
    Identifiers: TDeclaration_Identifiers;
    VarType: TDeclaration_Type;
    Value: TDeclaration_Value;
    Modifier: EParameterModifier;

    constructor Create(Parser: TLapeTools_Parser);
  end;

implementation

uses
  FileUtil;

const
  ParserTokens_BlockEnd = [tk_kw_Operator, tk_kw_Procedure, tk_kw_Function, tk_kw_Var, tk_kw_Const, tk_kw_Type, tk_kw_Begin, tk_kw_Label, tk_NULL];
  ParserTokens_Types = [tk_op_Deref, tk_kw_Array, tk_kw_Record, tk_kw_Packed, tk_kw_Function, tk_kw_Procedure,
                        tk_kw_External, tk_kw_Private, tk_kw_Type, tk_kw_Set, tk_Identifier, tk_sym_ParenthesisOpen];

function TDeclaration_Parameters.Get(Index: Int32): TDeclaration_Parameter;
begin
  Result := FItems[Index];
end;

function TLapeTools_Parser.FindFile(AFilePath: lpString): lpString;
var
  i: Int32;
  Dir: lpString;
begin
  if (AFilePath = '') then
    Exit('');

  AFilePath := SetDirSeparators(AFilePath);
  if FileExists(AFilePath) then
    Exit(AFilePath);

  if FileExists(ExpandFileName(AFilePath)) then
    Exit(ExpandFileName(AFilePath));
  if FileExists(IncludeTrailingPathDelimiter(ExtractFileDir(AFilePath))) then
    Exit(IncludeTrailingPathDelimiter(ExtractFileDir(AFilePath)));

  for i := FTokenizer downto 0 do
    if (FTokenizers[i] <> nil) then
    begin
      Dir := ExtractFilePath(FTokenizers[i].FileName);
      if FileExists(string(Dir + AFilePath)) then
        Exit(SetDirSeparators(Dir + AFilePath));
    end;

  for i := 0 to FPaths.Count - 1 do
  begin
    Dir := IncludeTrailingPathDelimiter(FPaths[i]);
    if FileExists(string(Dir + AFilePath)) then
      Exit(SetDirSeparators(Dir + AFilePath));
   end;

  Exit('');
end;

function TLapeTools_Parser.HasDeclaration(Name: lpString; LocalOnly: Boolean; CheckWith: Boolean): Boolean;
begin
  Result := FMap.ExistsKey(Name);
end;

function TLapeTools_Parser.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
var
  Path: lpString;
  Declaration: TDeclaration_Include;
begin
  if (not InIgnore()) then
    case LowerCase(Directive) of
      'i', 'include', 'include_once':
        begin
          Path := FindFile(Argument);

          if FileExists(Path) then
          begin
            if (not Tokenizer.InPeek) then
            begin
              Declaration := TDeclaration_Include.Create(Self, True);
              Declaration.Text := ExtractFileName(Declaration.Text);
              Declaration.Path := Path;

              FMap.Add(Declaration.Text, Declaration);
            end;

            Argument := Path;
          end;

          if (not (psParseIncludes in FSettings)) then
            Exit(True);
        end;

      'loadlib':
        if (psParseIncludes in FSettings) and (not Tokenizer.InPeek) and (FOnLibraryDirective <> nil) then
        begin
          Path := FOnLibraryDirective(Argument);
          if (Path <> '') then
            pushTokenizer(TLapeTokenizerString.Create(Path, ExtractFileName(Argument)));
        end;
    end;

  try
    inherited HandleDirective(Sender, Directive, Argument);
  except
    on e: Exception do
      WriteLn('[PARSER]: ', e.Message);
  end;

  Exit(True);
end;

function TLapeTools_Parser.GetType(Declaration: TDeclaration): TDeclaration_Type;
begin
  Result := nil;

  if (Declaration is TDeclaration_Type_Identifier) then
  begin
    if (FInMethod <> nil) and (FInMethod.Locals.GetType(Declaration.Text) <> nil) then
      Declaration := FInMethod.Locals.GetType(Declaration.Text)
    else
      Declaration := FMap.GetType(Declaration.Text);
  end;

  if (Declaration is TDeclaration_Variable) then
    with Declaration as TDeclaration_Variable do
    begin
      if (VarType <> nil) then
        Result := GetType(VarType);
    end
    else
    if (Declaration is TDeclaration_Method) then
      with Declaration as TDeclaration_Method do
      begin
        if (Header.Result <> nil) then
          Result := GetType(Header.Result);
      end
    else
    if (Declaration is TDeclaration_Type_Method) then
      with Declaration as TDeclaration_Type_Method do
      begin
        if (Header.Result <> nil) then
          Result := GetType(Header.Result);
      end;

   if (Result = nil) and (Declaration is TDeclaration_Type) then
      Result := Declaration as TDeclaration_Type;
end;

function TLapeTools_Parser.Doc: lpString;
begin
  Result := TLapeTokenizerString(Tokenizer).Doc;
end;

function TLapeTools_Parser.DocPos: TDocPos;
begin
  Result := getDocPos();
end;

function TLapeTools_Parser.Read(Tokens: Int32; IgnoreTokens: EParserTokenSet): lpString;
var
  i: Int32;
  Token: EParserToken;
begin
  Result := '';

  Token := Tokenizer.Tok;
  if (not Tokenizer.InPeek) and (not InIgnore()) and (not (Token in IgnoreTokens)) then
    Result := Tokenizer.TokString;

  for i := 1 to Tokens do
  repeat
    Token := Tokenizer.Next();

    if (Token = tk_NULL) and (FTokenizer > 0) then
    begin
      if Tokenizer.InPeek then
        Dec(FTokenizer)
      else
        popTokenizer();

     Token := Tokenizer.Next();
    end;

    if (not Tokenizer.InPeek) and (not InIgnore()) and (not (Token in IgnoreTokens)) then
      Result := Result + Tokenizer.TokString;
  until (Token = tk_NULL) or ((not (Token in TokJunk)) and (not InIgnore()));
end;

type
  __LapeTokenizerBase = class(TLapeTokenizerBase);

function TLapeTools_Parser.Next: EParserToken;
var
  PrevTok: EParserToken;
  i: Int32;
begin
  PrevTok := Tokenizer.Tok;
  repeat
    Result := Tokenizer.Next();

    if (Result = tk_NULL) and (FTokenizer > 0) then
    begin
      if Tokenizer.InPeek then
        Dec(FTokenizer)
      else
        popTokenizer();

      Result := Tokenizer.Next();
    end;

    if (not Tokenizer.InPeek) and (not InIgnore()) then
    begin
      if (Result = tk_Comment) then
      begin
        FComment.Start := Tokenizer.TokStart;
        FComment.Len := Tokenizer.TokLen;
      end;

      if (FCaret.Pos > 0) and (not FCaret.Reached) and (FTokenizer = 0) and (Tokenizer.Pos >= FCaret.Pos) then
        FCaret.Reached := True;
    end;
  until (Result = tk_NULL) or ((not (Result in TokJunk)) and (not InIgnore()));
  __LapeTokenizerBase(Tokenizer).FLastTok := PrevTok;

  if (not Tokenizer.InPeek) then
    for i := 0 to FStack.Count - 1 do
      FStack[i].Tokens := FStack[i].Tokens + 1;
end;

function TLapeTools_Parser.Tok: EParserToken;
begin
  Result := Tokenizer.Tok;
end;

function TLapeTools_Parser.TokStart: Int32;
begin
  Result := Tokenizer.TokStart;
end;

function TLapeTools_Parser.TokString: lpString;
begin
  Result := Tokenizer.TokString;
end;

function TLapeTools_Parser.Find(Name: lpString): TDeclaration;

  function FindInMethods(Methods: TDeclaration_Methods): TDeclaration;
  var
    i: Int32;
  begin
    for i := 0 to High(Methods) do
      if UpperCase(Name) = UpperCase(Methods[i].Header.Name.Text) then
        Exit(Methods[i]);

    Exit(nil);
  end;

  function FindInType(Declaration: TDeclaration; isParent: Boolean = False): TDeclaration;
  var
    Parent: TDeclaration_Type;
  begin
    Result := nil;

    if (Declaration <> nil) and (Declaration is TDeclaration_Type) then
    begin
      if Declaration is TDeclaration_Type_Record then
        Result := TDeclaration_Type_Record(Declaration).Fields[Name];

      if (Result = nil) and (TDeclaration_Type(Declaration).Name <> nil) then
        Result := FindInMethods(FMap.GetMethods(TDeclaration_Type(Declaration).Name.Text));

      if (Result = nil) then
        for Parent in TDeclaration_Type(Declaration).Parents(Self) do
        begin
          Result := FindInType(Parent, True);
          if (Result <> nil) then
            Exit;
        end;
      end;
  end;

  function FindInMethod(Method: TDeclaration_Method): TDeclaration;
  begin
    Result := nil;

    if (UpperCase(Name) = 'RESULT') and (Method.Header.MethodType in [mtFunction, mtFunctionOfObject]) then
      Result := Method.Header.Result
    else
    if (UpperCase(Name) = 'SELF') and (Method.Header.MethodType in [mtFunctionOfObject, mtProcedureOfObject]) then
      Result := Method.Header.ObjectName
    else
    begin
      Result := Method.Locals[Name];
      if (Result = nil) then
        Result := Method.Header.Parameters[Name];
    end;

    if (Result = nil) and (Method.Header.MethodType in [mtFunctionOfObject, mtProcedureOfObject]) then
      Result := FindInType(FMap.GetType(Method.Header.ObjectName.Text));

    if (Result = nil) and (Method.Parent <> nil) and (not (mdStatic in Method.Header.Directives)) then
      Result := FindInMethod(Method.Parent);
  end;

  function FindEnum: TDeclaration;
  var
    i: Int32;
    Enum: TDeclaration_Type_Enum;
  begin
    Result := nil;

    for i := 0 to FMap.Count - 1 do
      if (FMap.Get(i) is TDeclaration_Type_Enum) then
      begin
        Enum := FMap.Get(i) as TDeclaration_Type_Enum;

        Result := Enum.Elements[Name];
        if (Result <> nil) then
          Break;
      end;
  end;

begin
  Result := nil;

  if (FInMethod <> nil) then
    Result := FindInMethod(FInMethod);
  if (Result = nil) then
    Result := FMap[Name];
  if (Result = nil) then
    Result := FindEnum();
end;

function TLapeTools_Parser.ParseExpression(Expression: lpString; ReturnType: Boolean): TDeclaration;
type
  TExpressionItems = array of record Name: lpString; Dimensions: Int32; end;
var
  Items: TExpressionItems;
  Item: Int32;

  function GetItems: TExpressionItems;
  var
    InParameters, InBrackets: Int32;
    Tokenizer: TLapeTokenizerString;
  begin
    SetLength(Result, 1);

    InParameters := 0;
    InBrackets := 0;

    Tokenizer := TLapeTokenizerString.Create(Expression);

    with Tokenizer do
    try
      while (Next() <> tk_NULL) do
      begin
        case Tok of
          tk_sym_ParenthesisOpen:
            Inc(InParameters);
          tk_sym_ParenthesisClose:
            Dec(InParameters);
          tk_sym_BracketClose:
            if (InParameters = 0) then
              Dec(InBrackets);
          tk_sym_BracketOpen:
            if (InParameters = 0) then
            begin
              Inc(Result[High(Result)].Dimensions);
              Inc(InBrackets);
            end;
          tk_sym_Comma:
            if (InBrackets > 0) then
              Inc(Result[High(Result)].Dimensions);
          tk_sym_Dot:
            if (InParameters = 0) and (InBrackets = 0) then
              SetLength(Result, Length(Result) + 1);
          else
            if (InParameters = 0) and (InBrackets = 0) and (not (Tok in TokJunk)) then
              Result[High(Result)].Name := Result[High(Result)].Name + TokString;
        end;
      end;
    except
      { nothing }
    end;

    Tokenizer.Free();

    if (Result[High(Result)].Name = '') then
      SetLength(Result, Length(Result) - 1);
  end;

  function GetStartingType(Name: lpString): TDeclaration_Type;
  begin
    Result := GetType(Find(Name));
  end;

  function GetArray(Declaration: TDeclaration; Dimensions: Int32): TDeclaration_Type;
  var
    i: Int32 = 0;
  begin
    Result := nil;

    while (i < Dimensions) and (Declaration <> nil) and (Declaration is TDeclaration_Type_Array) do
      with TDeclaration_Type_Array(Declaration) do
      begin
        i := i + Dimensions;
        Declaration := GetType(ArrayType);
      end;

    if (i = Dimensions) then
      Result := Declaration as TDeclaration_Type;
  end;

  function SearchType(Declaration: TDeclaration_Type; Name: lpString; isParent: Boolean = False): TDeclaration;

     function SearchFields(Declaration: TDeclaration_Type_Record): TDeclaration;
     begin
       Result := nil;

       if (Declaration.Fields[Name] <> nil) then
       begin
         if (not ReturnType) and (Item = High(Items)) then
            Result := Declaration.Fields[Name]
          else
            Result := GetType(Declaration.Fields[Name]);
       end;
     end;

     function SearchMethods(Declaration: TDeclaration_Type): TDeclaration;
     var
       Method: TDeclaration_Method;
     begin
       Result := nil;
       for Method in FMap.GetMethods(Declaration.Name.Text) do
          if (LapeCase(Method.Header.Name.Text) = LapeCase(Name)) then
          begin
            if (not ReturnType) and (Item = High(Items)) then
              Exit(Method);
            if (Method.Header.MethodType = mtFunctionOfObject) then
              Exit(GetType(Method.Header.Result));
          end;
     end;

  var
    Parent: TDeclaration_Type;
  begin
    Result := nil;

    if (Declaration <> nil) then
    begin
      if (Declaration is TDeclaration_Type_Record) then
        Result := SearchFields(Declaration as TDeclaration_Type_Record);

      if (Result = nil) and (Declaration.Name <> nil) then
        Result := SearchMethods(Declaration);

      if (Result = nil) and (not isParent) then
        for Parent in Declaration.Parents(Self) do
          begin
            Result := SearchType(Parent, Name, True);
            if (Result <> nil) then
              Exit;
          end;
    end;
  end;

begin
  Result := nil;

  Items := GetItems();

  if (Length(Items) > 0) then
  begin
    for Item := 0 to High(Items) do
    begin
      if (Item = 0) then
      begin
        Result := GetStartingType(Items[Item].Name);
        if (Result <> nil) and (Items[Item].Dimensions > 0) then
          Result := GetArray(Result, Items[Item].Dimensions);
      end else
      begin
        Result := SearchType(Result as TDeclaration_Type, Items[Item].Name);
        if (Result <> nil) and (Items[Item].Dimensions > 0) then
          Result := GetArray(Result, Items[Item].Dimensions);
      end;

      if (Result = nil) then
        Break;
    end;
  end;
end;

procedure TLapeTools_Parser.Parse;
begin
  try
    Next();

    while (Tok() <> tk_NULL) do
    begin
      case Tok() of
        tk_kw_Var:
          FMap.ParseVariables(Self);
        tk_kw_Const:
          FMap.ParseConstants(Self);
        tk_kw_Type:
          FMap.ParseTypes(Self);
        tk_kw_Label:
          FMap.ParseLabels(Self);
        tk_kw_Function, tk_kw_Procedure, tk_kw_Operator:
          FMap.ParseMethod(Self);
        else
          Next();
      end;
    end;
  except
    on e: Exception do
      WriteLn(e.Message);
  end;
end;

constructor TLapeTools_Parser.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean; AEmitter: TLapeCodeEmitter; ManageEmitter: Boolean);
begin
  FreeTokenizer := ManageTokenizer;

  FIncludes := TStringList.Create();
  FIncludes.Duplicates := dupIgnore;
  FIncludes.CaseSensitive := LapeSystemCaseSensitive;

  FDefines := TStringList.Create();
  FDefines.Duplicates := dupIgnore;
  FDefines.CaseSensitive := LapeCaseSensitive;

  FBaseDefines := TStringList.Create();
  FBaseDefines.CaseSensitive := LapeCaseSensitive;

  FConditionalStack := TLapeConditionalStack.Create(0);

  setTokenizer(ATokenizer);

  Reset();
end;

constructor TLapeTools_Parser.Create(ADoc: lpString; AFilePath: lpString; ACaret: Int32);
begin
  Create(TLapeTokenizerString.Create(ADoc, AFilePath));

  FStack := TDeclarationStack.Create();
  FData := TDeclarationList.Create(nil, dupAccept, False);
  FMap := TDeclarationMap.Create(nil, dupAccept, False);

  FPaths := TStringList.Create();
  FFilePath := AFilePath;
  FFileAge := SysUtils.FileAge(FFilePath);

  FSettings := [psParseIncludes, psAddMethodUnderType];

  FCaret.Pos := ACaret;
  if (FCaret.Pos = 0) then
    FCaret.Reached := True;
end;

constructor TLapeTools_Parser.Create(AFilePath: lpString);
begin
  Create(TLapeTokenizerFile.Create(AFilePath));

  FStack := TDeclarationStack.Create();
  FData := TDeclarationList.Create(nil, dupAccept, False);
  FMap := TDeclarationMap.Create(nil, dupAccept, False);

  FPaths := TStringList.Create();
  FFilePath := AFilePath;
  FFileAge := SysUtils.FileAge(FFilePath);

  FSettings := [psParseIncludes, psAddMethodUnderType];
end;

destructor TLapeTools_Parser.Destroy;
var
  i: Int32;
begin
  for i := 0 to FData.Count - 1 do
    FData[i].Free();
  for i := 0 to High(FTokenizers) do
    FTokenizers[i].Free();

  FPaths.Free();
  FStack.Free();
  FData.Free();
  FMap.Free();

  FIncludes.Free();
  FDefines.Free();
  FBaseDefines.Free();
  FConditionalStack.Free();
end;

function TDeclaration_Include.getDocPos: TDocPos;
begin
  Result := NullDocPos;

  if FileExists(Path) then
  begin
    Result.FileName := Path;
    Result.Col := 1;
    Result.Line := 0;
  end;
end;

function TDeclaration_Include.getText: String;
begin
  Result := inherited getText();
  if (Result <> '') and (Result[Length(Result)] = '}') then
    SetLength(Result, Length(Result) - 1);
end;

function TDeclaration_Label.getDocPos: TDocPos;
begin
  if (Name <> nil) then
    Result := Name.DocPos
  else
    Result := inherited getDocPos();
end;

constructor TDeclaration_Label.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, False);
end;

constructor TDeclaration_Type_Pointer.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    PointerType := TDeclaration_Type_Identifier.Create(Parser);
  end;
end;

constructor TDeclaration_Type_Copy.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    CopyType := TDeclaration_Type_Identifier.Create(Parser);
  end;
end;

constructor TDeclaration_Type_Array.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  with Parser do
  begin
    while (Tok() = tk_kw_Array) do
    begin
      case Expect([tk_kw_Of, tk_sym_BracketOpen], True, False) of
        tk_kw_Of:
          Next();
        tk_sym_BracketOpen:
          begin
            while (not (Tok() in [tk_sym_BracketClose, tk_NULL])) do
              Next();

            Expect(tk_kw_Of, True, True);
          end;
      end;

      Inc(Dimensions);
    end;
  end;

  ArrayType := TDeclaration_Type.Identify(Parser, True).Create(Parser);
end;

constructor TDeclaration_Type_Method.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  Header := TDeclaration_MethodHeader.Create(Parser);
end;

constructor TDeclaration_Type_Set.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  with Parser do
  begin
    Expect(tk_kw_Set, False, True);
    Expect(tk_kw_Of, False, True);

    SetType := TDeclaration_Value.Create(Parser);
  end;
end;

function TDeclaration_Type_Enum.getDocPos: TDocPos;
begin
  if (Name <> nil) then
    Result := Name.DocPos
  else
    Result := inherited getDocPos();
end;

constructor TDeclaration_Type_Enum.Create(Parser: TLapeTools_Parser);
var
  Element: TDeclaration_EnumElement;
begin
  inherited Create(Parser);

  Elements := TDeclarationMap.Create(nil, dupAccept, False);

  with Parser do
  begin
    while (Tok() <> tk_sym_ParenthesisClose) do
    begin
      Expect(tk_Identifier, True, False);

      Element := TDeclaration_EnumElement.Create(Parser);
      Element.Enum := Self;
      Elements.Add(Element.Name.Text, Element);
    end;

    Expect(tk_sym_ParenthesisClose, False, True);
  end;
end;

destructor TDeclaration_Type_Enum.Destroy;
begin
  Elements.Free();

  inherited Destroy();
end;

function TDeclaration_EnumElement.getDocPos: TDocPos;
begin
  if (Name <> nil) then
    Result := Name.DocPos
  else
    Result := inherited getDocPos();
end;

constructor TDeclaration_EnumElement.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, False);

  with Parser do
  begin
    Name := TDeclaration_Identifier.Create(Parser);

    if (Tok() = tk_sym_Equals) then
    begin
      Next();

      Value := TDeclaration_Value.Create(Parser);
    end;
  end;
end;

constructor TDeclaration_Type_Alias.Create(Parser: TLapeTools_Parser);
const
  tk_kw_Native = 'NATIVE';
  tk_kw_Lapify = 'LAPIFY';
begin
  inherited Create(Parser);

  with Parser do
  begin
    // https://youtu.be/3tmd-ClpJxA
    if (UpperCase(TokString()) = tk_kw_Native) or (UpperCase(TokString()) = tk_kw_Lapify) then
    begin
      // native TProc;
      // native(TProc);
      // native(TProc, FFI_CDECL);

      case Expect([tk_Identifier, tk_sym_ParenthesisOpen], True, False) of
        tk_Identifier:
          AliasType := TDeclaration_Type_Identifier.Create(Parser);

        tk_sym_ParenthesisOpen:
          begin
            Expect(tk_Identifier, True, False);

            AliasType := TDeclaration_Type_Identifier.Create(Parser);

            case Expect([tk_sym_ParenthesisClose, tk_sym_Comma], False, True) of
              tk_sym_Comma:
                Expect(tk_sym_ParenthesisClose, True, True);
            end;
          end;
      end;
   end else
     AliasType := TDeclaration_Type_Identifier.Create(Parser);
  end;
end;

function TDeclaration.getDocPos: TDocPos;
begin
  if (FState <> nil) and (FDocPos.Col = NullDocPos.Col) and (FDocPos.Line = NullDocPos.Line) then
    with FParser do
    begin
      setState(FState, False);

      FDocPos := DocPos();
    end;

  Result := FDocPos;
end;

function TDeclaration.getText: String;
begin
  if (FText = '') and (FState <> nil) then
    with FParser do
    begin
      setState(FState, False);

      FText := Read(FTokens - 1, [tk_Comment, tk_Directive]);
    end;

  Result := FText;
end;

procedure TDeclaration.AfterConstruction;
begin
  FParser.Data.Add(Self);
  FParser.Stack.Pop();
end;

constructor TDeclaration.Create(Parser: TLapeTools_Parser; getState: Boolean);
begin
  FParser := Parser;
  FDocPos := NullDocPos;
  FInScope := (FParser.Caret.Pos = 0) or (not FParser.Caret.Reached);
  if getState then
    FState := Parser.getState();

  Parser.Stack.Push(Self);
end;

destructor TDeclaration.Destroy;
begin
  if (FState <> nil) then
    with FParser do
      freeState(FState);
end;

constructor TDeclaration_Identifier.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, True);

  with FParser do
  begin
    FText := TokString;

    Next();
  end;
end;

function TDeclaration_Type.getDocPos: TDocPos;
begin
  if (Name <> nil) then
    Result := Name.DocPos
  else
    Result := inherited getDocPos();
end;

function TDeclaration_Type.Parents(Parser: TLapeTools_Parser): TDeclaration_Types;

  function GetParent(Declaration: TDeclaration_Type): TDeclaration_Type;
  begin
    Result := nil;

    if Declaration is TDeclaration_Type_Copy then
      Result := TDeclaration_Type_Copy(Declaration).CopyType
    else
    if Declaration is TDeclaration_Type_Alias then
      Result := TDeclaration_Type_Alias(Declaration).AliasType
    else
    if Declaration is TDeclaration_Type_Record then
      Result := TDeclaration_Type_Record(Declaration).Parent;

    if (Result <> nil) then
      Result := Parser.Map.GetType(Result.Text);
  end;

var
  Declaration: TDeclaration_Type;
begin
  SetLength(Result, 0);

  Declaration := GetParent(Self);

  while (Declaration <> nil) and (Declaration <> Self) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Declaration;

    Declaration := GetParent(Declaration);
  end;
end;

class function TDeclaration_Type.Identify(Parser: TLapeTools_Parser; VarType: Boolean): TDeclaration_Type_Identity;
begin
  with Parser do
  begin
    case Expect(ParserTokens_Types, False, False) of
      tk_kw_Record, tk_kw_Packed:
        Result := TDeclaration_Type_Record;
      tk_sym_ParenthesisOpen:
        Result := TDeclaration_Type_Enum;
      tk_kw_Set:
        Result := TDeclaration_Type_Set;
      tk_kw_Procedure, tk_kw_Function:
        Result := TDeclaration_Type_Method;
      tk_kw_Array:
        Result := TDeclaration_Type_Array;
      tk_kw_Type:
        Result := TDeclaration_Type_Copy;
      tk_sym_Caret:
        Result := TDeclaration_Type_Pointer;
      tk_Identifier:
        if VarType then
          Result := TDeclaration_Type_Identifier
        else
          Result := TDeclaration_Type_Alias;
    end;
  end;
end;

constructor TDeclaration_Type.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, True);
end;

constructor TDeclaration_Type_Identifier.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser);

  with FParser do
  begin
    FText := TokString;

    Next();
  end;
end;

constructor TDeclaration_Type_Record.Create(Parser: TLapeTools_Parser);
var
  Identifier: TDeclaration_Identifier;
  Field: TDeclaration_Field;
begin
  inherited Create(Parser);

  Fields := TDeclarationMap.Create(nil, dupAccept, False);

  with Parser do
  begin
    if (Tok() = tk_kw_Packed) then
      Next();

    case Expect([tk_Identifier, tk_sym_ParenthesisOpen], True, False) of
      tk_sym_ParenthesisOpen:
        begin
          Expect(tk_Identifier, True, False);
          Parent := TDeclaration_Type_Identifier.Create(Parser);
          Expect(tk_Identifier, True, False);
        end;
    end;

    repeat
      with TVariableParser.Create(Parser) do
      try
        for Identifier in Identifiers do
	begin
	  Field := TDeclaration_Field.Create(Parser);
          Field.Name := Identifier;
	  Field.VarType := VarType;

          Fields.Add(Field.Name.Text, Field);
	end;
      finally
        Free();
      end;

      Expect(tk_sym_SemiColon, False, True);
    until (Tok() = tk_kw_End);

    Expect(tk_kw_End, False, True);
  end;
end;

destructor TDeclaration_Type_Record.Destroy;
begin
  Fields.Free();

  inherited Destroy();
end;

constructor TDeclaration_Value.Create(Parser: TLapeTools_Parser);
var
  Parenthesis, Brackets: Int32;
begin
  inherited Create(Parser, True);

  with Parser do
  begin
    Parenthesis := 0;
    Brackets := 0;

    while (Tok() <> tk_NULL) and ((Parenthesis > 0) or (Brackets > 0) or (not (Tok() in [tk_sym_SemiColon, tk_sym_ParenthesisClose, tk_sym_Comma]))) do
    begin
      case Tok() of
        tk_sym_ParenthesisOpen:
          Inc(Parenthesis);
        tk_sym_ParenthesisClose:
          Dec(Parenthesis);
        tk_sym_BracketOpen:
          Inc(Brackets);
        tk_sym_BracketClose:
          Dec(Brackets);
      end;

      Next();
    end;
  end;
end;

function TDeclaration_Variable.getDocPos: TDocPos;
begin
  if (Name <> nil) then
    Result := Name.DocPos
  else
    Result := inherited getDocPos();
end;

constructor TDeclaration_Variable.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, False);
end;

function TDeclaration_Method.getDocPos: TDocPos;
begin
  if (Header.Name <> nil) then
    Result := Header.Name.DocPos
  else
    Result := inherited getDocPos();
end;

function TDeclaration_Method.Description: lpString;
var
  i, j, p: Int32;
begin
  Result := '';

  if (Documentation <> '') then
    with TStringList.Create() do
    try
      Text := Documentation;

      for i := 0 to Count - 1 do
      begin
        p := Pos('@desc:', Strings[i]);

        if (p > 0) then
        begin
          for j := i to Count - 2 do
            Result := Result + Copy(Strings[j], p + Length('@desc: '), MaxInt) + LineEnding;
          Result := TrimRight(Result);

          Break;
        end;
      end;
    finally
      Free();
    end;
end;

constructor TDeclaration_Method.Create(Parser: TLapeTools_Parser);
begin
  inherited Create(Parser, False);

  Locals := TDeclarationMap.Create(nil, dupAccept, False);

  with Parser do
    if (Comment.Start + Comment.Len) = (TokStart() - Length(LineEnding)) then
      Documentation := Copy(Doc(), Comment.Start, Comment.Len + 1);
end;

destructor TDeclaration_Method.Destroy;
begin
  Locals.Free();

  inherited Destroy();
end;

constructor TDeclaration_MethodHeader.Create(Parser: TLapeTools_Parser);
var
  Identifier: TDeclaration_Identifier;
  Parameter: TDeclaration_Parameter;
  Group: Int32;
begin
  inherited Create(Parser, True);

  Parameters := TDeclaration_Parameters.Create(nil, dupAccept, False);
  Group := 0;

  with Parser do
  begin
    case Expect([tk_Identifier, tk_sym_ParenthesisOpen, tk_sym_ParenthesisClose, tk_sym_Colon, tk_sym_SemiColon, tk_kw_Of] + ParserToken_Operators, True, False) of
      tk_Identifier:
        begin
          if (Peek() = tk_sym_Dot) then
          begin
            MethodType := mtProcedureOfObject;
            ObjectName := TDeclaration_Type_Identifier.Create(Parser);

            Expect(tk_Identifier, True, False);
          end;

          Name := TDeclaration_Identifier.Create(Parser);
        end;
      ParserToken_FirstOperator..ParserToken_LastOperator:
        begin
          MethodType := mtOperator;
          Name := TDeclaration_Identifier.Create(Parser);
        end;
    end;

    case Expect([tk_sym_ParenthesisOpen, tk_sym_ParenthesisClose, tk_sym_SemiColon, tk_sym_Colon, tk_kw_Of], False, False) of
      tk_sym_ParenthesisOpen:
        begin
          Next();

          while (not (Tok() = tk_sym_ParenthesisClose)) do
          begin
            with TVariableParser.Create(Parser) do
            try
              for Identifier in Identifiers do
	      begin
	        Parameter := TDeclaration_Parameter.Create(Parser);
                Parameter.Name := Identifier;
                Parameter.VarType := VarType;
                Parameter.Value := Value;
                Parameter.Group := Group;
                Parameter.Modifier := Modifier;

                Parameters.Add(Parameter.Name.Text, Parameter);
	      end;
            finally
              Free();
            end;

            Inc(Group);

            case Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False) of
              tk_sym_SemiColon:
                Next();
            end;
          end;

          if (Tok() = tk_sym_ParenthesisClose) then
            Next();
        end;
    end;

    case Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose, tk_sym_Colon, tk_kw_Of], False, False) of
      tk_sym_Colon:
        begin
          Next();

          if (MethodType = mtProcedureOfObject) then
            MethodType := mtFunctionOfObject
          else
            MethodType := mtFunction;

          Result := TDeclaration_Type.Identify(Parser, True).Create(Parser);
        end;
    end;

    if (Tok() = tk_kw_Of) then
      Expect(tk_kw_Object, True, True);

    while (Peek() in [tk_kw_Overload, tk_kw_Override, tk_kw_Static, tk_kw_ConstRef, tk_kw_Forward, tk_kw_External]) do
      case Expect([tk_kw_Overload, tk_kw_Override, tk_kw_Static, tk_kw_ConstRef, tk_kw_Forward, tk_kw_External], True, True) of
        tk_kw_Overload:
          Include(Directives, mdOverload);
        tk_kw_Override:
          Include(Directives, mdOverride);
        tk_kw_Static:
          Include(Directives, mdStatic);
        tk_kw_Forward:
          Include(Directives, mdForward);
        tk_kw_External:
          begin
            Include(Directives, mdExternal);

            while (not (Tok() in [tk_sym_SemiColon, tk_NULL])) do
              Next();
            Expect(tk_sym_SemiColon, False, False);
          end;
      end;
  end;
end;

destructor TDeclaration_MethodHeader.Destroy;
begin
  Parameters.Free();

  inherited Destroy();
end;

procedure TDeclarationMap_Helper.ParseVariables(Parser: TLapeTools_Parser);
var
  Variable: TDeclaration_Variable;
  Identifier: TDeclaration_Identifier;
begin
  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    repeat
      with TVariableParser.Create(Parser) do
      try
        for Identifier in Identifiers do
	begin
	  Variable := TDeclaration_Variable.Create(Parser);
          Variable.Name := Identifier;
	  Variable.VarType := VarType;
	  Variable.Value := Value;

          Add(Variable.Name.Text, Variable);
	end;
      finally
        Free();
      end;

      Expect(tk_sym_SemiColon, False, True);
    until (Tok() in ParserTokens_BlockEnd);
  end;
end;

procedure TDeclarationMap_Helper.ParseConstants(Parser: TLapeTools_Parser);
var
  Constant: TDeclaration_Constant;
  Identifier: TDeclaration_Identifier;
begin
  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    repeat
      with TVariableParser.Create(Parser) do
      try
        for Identifier in Identifiers do
        begin
          Constant := TDeclaration_Constant.Create(Parser);
          Constant.Name := Identifier;
          Constant.VarType := VarType;
          Constant.Value := Value;

          Add(Constant.Name.Text, Constant);
        end;
      finally
        Free();
      end;

      Expect(tk_sym_SemiColon, False, True);
    until (Tok() in ParserTokens_BlockEnd);
  end;
end;

procedure TDeclarationMap_Helper.ParseTypes(Parser: TLapeTools_Parser);
var
  Name: TDeclaration_Identifier;
  Declaration: TDeclaration_Type;
begin
  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    repeat
      Expect(tk_Identifier, False, False);
      Name := TDeclaration_Identifier.Create(Parser);
      Expect(tk_sym_Equals, False, True);

      Declaration := TDeclaration_Type.Identify(Parser, False).Create(Parser);
      Declaration.Name := Name;

      Add(Declaration.Name.Text, Declaration);

      Expect(tk_sym_SemiColon, False, True);
    until (Tok() in ParserTokens_BlockEnd);
  end;
end;

procedure TDeclarationMap_Helper.ParseLabels(Parser: TLapeTools_Parser);
var
  Identifier: TDeclaration_Identifier;
  Declaration: TDeclaration_Label;
begin
  with Parser do
  begin
    Expect(tk_Identifier, True, False);

    with TVariableParser.Create(Parser) do
    try
      for Identifier in Identifiers do
      begin
        Declaration := TDeclaration_Label.Create(Parser);
        Declaration.Name := Identifier;

        Add(Declaration.Name.Text, Declaration);
      end;
    finally
      Free();
    end;

    Expect(tk_sym_SemiColon, False, True);
  end;
end;

function TDeclarationMap_Helper.ParseMethod(Parser: TLapeTools_Parser): TDeclaration_Method;
var
  Blocks, StartPos: Int32;
begin
  Result := TDeclaration_Method.Create(Parser);
  Result.Header := TDeclaration_MethodHeader.Create(Parser);

  if (not (mdForward in Result.Header.Directives)) and (not (mdExternal in Result.Header.Directives)) then
    with Parser do
    try
      StartPos := TokStart();

      while (Tok() <> tk_kw_Begin) do
      begin
        if (Tok() = tk_sym_SemiColon) then
          Next();

        case Expect([tk_kw_Var, tk_kw_Begin, tk_kw_Const, tk_kw_Function, tk_kw_Procedure, tk_kw_Type, tk_kw_Label], False, False) of
          tk_kw_Var:
            Result.Locals.ParseVariables(Parser);
          tk_kw_Const:
            Result.Locals.ParseConstants(Parser);
          tk_kw_Type:
            Result.Locals.ParseTypes(Parser);
          tk_kw_Label:
            Result.Locals.ParseLabels(Parser);
          tk_kw_Procedure, tk_kw_Function:
            Result.Locals.ParseMethod(Parser).Parent := Result;
        end;
      end;

      Expect(tk_kw_Begin, False, True);

      Blocks := 1;

      while (Blocks > 0) and (Tok() <> tk_NULL) do
      begin
        case Tok() of
          tk_kw_Begin, tk_kw_Try, tk_kw_Case:
            Inc(Blocks);
          tk_kw_End:
            Dec(Blocks);
          tk_kw_Procedure, tk_kw_Function: // Incompleted block, keep parsing for hopefully more type methods.
            Break;
        end;

        if (Blocks > 0) then
          Next();
      end;

      if (not (Tok() in [tk_kw_Procedure, tk_kw_Function])) then
        Expect(tk_kw_End, False, True);
    finally
      if (FInMethod = nil) and (Caret.Reached) and (Caret.Pos > StartPos) then
        FInMethod := Result;
    end;

  if (not (mdForward in Result.Header.Directives)) and (Result.Header.Name <> nil) then
  begin
   if (Result.Header.MethodType in [mtProcedureOfObject, mtFunctionOfObject]) then
    begin
      Add(Result.Header.ObjectName.Text + '.' + Result.Header.Name.Text, Result);

      if (psAddMethodUnderType in Parser.Settings) then
        Add(Result.Header.ObjectName.Text, Result);
    end else
      Add(Result.Header.Name.Text, Result);
  end;
end;

function TDeclarationMap_Helper.GetMethods(Name: lpString): TDeclaration_Methods;

  function getInherited(Method: lpString; Methods: TDeclaration_Methods): Int32;
  var
    i: Int32;
  begin
    Result := -1;

    for i := 0 to High(Methods) do
      if (Methods[i].Header.Name.Text = Method) and (not (mdOverride in Methods[i].Header.Directives)) then
        Exit(i);
  end;

var
  Indices: TIntegerArray;
  i: Int32;
  Method: TDeclaration_Method;
begin
  SetLength(Result, 0);
  Indices := IndicesOfKey(Name);

  for i := 0 to High(Indices) do
    if (FItems[Indices[i]] is TDeclaration_Method) then
    begin
      Method := FItems[Indices[i]] as TDeclaration_Method;

      if (mdOverride in Method.Header.Directives) then
      begin
        if getInherited(Method.Header.Name.Text, Result) > -1 then
          Result[getInherited(Method.Header.Name.Text, Result)] := Method;
      end else
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FItems[Indices[i]] as TDeclaration_Method;
      end;
    end;
end;

function TDeclarationMap_Helper.GetRecord(Name: lpString): TDeclaration_Type_Record;
var
  Indices: TIntegerArray;
  i: Int32;
begin
  Indices := IndicesOfKey(Name);

  for i := 0 to High(Indices) do
    if (FItems[Indices[i]] is TDeclaration_Type_Record) then
      Exit(FItems[Indices[i]] as TDeclaration_Type_Record);

  Exit(nil);
end;

function TDeclarationMap_Helper.GetType(Name: lpString): TDeclaration_Type;
var
  Indices: TIntegerArray;
  i: Int32;
begin
  Indices := IndicesOfKey(Name);

  for i := 0 to High(Indices) do
    if (FItems[Indices[i]] is TDeclaration_Type) then
      Exit(FItems[Indices[i]] as TDeclaration_Type);

  Exit(nil);
end;

function TDeclarationMap_Helper.Get(Index: Int32): TDeclaration;
begin
  Result := FItems[Index];
end;

constructor TVariableParser.Create(Parser: TLapeTools_Parser);
var
  Identifier: TDeclaration_Identifier;
begin
  with Parser do
  begin
    if (Tok() <> tk_Identifier) then
    begin
      case Tok() of
        tk_kw_Out:
          Modifier := pmOut;
        tk_kw_Var:
          Modifier := pmVar;
        tk_kw_Const:
          Modifier := pmConst;
        tk_kw_ConstRef:
          Modifier := pmConstRef;
      end;

      Expect(tk_Identifier, True, False);
    end;

    while (Tok() = tk_Identifier) do
    begin
      Identifier := TDeclaration_Identifier.Create(Parser);

      SetLength(Identifiers, Length(Identifiers) + 1);
      Identifiers[High(Identifiers)] := Identifier;

      if (Tok() = tk_sym_Comma) then
        Expect(tk_Identifier, True, False);
    end;

    VarType := nil;
    Value := nil;

    if (not (Tok() in [tk_sym_SemiColon, tk_sym_ParenthesisClose])) then
      case Expect([tk_sym_Colon, tk_sym_Equals, tk_op_Assign], False, True) of
        tk_sym_Colon:
          VarType := TDeclaration_Type.Identify(Parser, True).Create(Parser);
        tk_op_Assign, tk_sym_Equals:
          Value := TDeclaration_Value.Create(Parser);
      end;

    if (not (Tok() in [tk_sym_SemiColon, tk_sym_ParenthesisClose])) then
      case Expect([tk_sym_Equals, tk_op_Assign], False, True) of
        tk_sym_Equals, tk_op_Assign:
          Value := TDeclaration_Value.Create(Parser);
      end;
  end;
end;

end.

