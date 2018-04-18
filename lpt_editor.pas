unit lpt_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, LCLType, SynEdit, SynEditKeyCmds, Forms, Graphics, LMessages,
  lpt_parser, lpt_scriptparser;

const
  lecAutoComplete = ecUserFirst + 1;
  lecParameterHint = ecUserFirst + 2;
  lecFocusLost = ecUserFirst + 3;
  lecFocus = ecUserFirst + 4;
  lecCaretChange = ecUserFirst + 5;
  lecScroll = ecUserFirst + 6;
  lecEscape = ecUserFirst + 7;

type
  TLapeTools_ShowDeclaration = procedure(Line, Column: Int32; FilePath: String) of object;
  TLapeTools_CommandProcessor = procedure(var Command: TSynEditorCommand; Char: TUTF8Char) of object;
  TLapeTools_CommandProcessors = array of TLapeTools_CommandProcessor;
  TLapeTools_ScanCallback = function(Char: String; var Inside: Int32; Data: Pointer): Boolean;

  TLapeTools_Editor = class(TSynEdit)
  protected
    FFilePath: String;
    FPaths: TStringList;
    FImportsPath: String;
    FOnShowDeclaration: TLapeTools_ShowDeclaration;
    FCommandProcessors: TLapeTools_CommandProcessors;
    FOnLibraryDirective: TLapeTools_LibraryDirective;

    function Scan(XY: TPoint; StopXY: TPoint; Callback: TLapeTools_ScanCallback; Data: Pointer = nil): TPoint; overload;
    function Scan(XY: TPoint; Callback: TLapeTools_ScanCallback; Data: Pointer = nil): TPoint; overload;

    function GetChangeStamp: Int64;
    function GetScript: String;
    function GetCaret: Int32;
    function GetCaretChar: Char;

    procedure DoMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoClickLink(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoCaretChange(Sender: TObject);
  public
    property OnShowDeclaration: TLapeTools_ShowDeclaration read FOnShowDeclaration write FOnShowDeclaration;
    property OnLibraryDirective: TLapeTools_LibraryDirective read FOnLibraryDirective write FOnLibraryDirective;

    property ChangeStamp: Int64 read GetChangeStamp;
    property FilePath: String read FFilePath write FFilePath;
    property Paths: TStringList read FPaths;
    property ImportsPath: String read FImportsPath write FImportsPath;
    property Script: String read GetScript;
    property Caret: Int32 read GetCaret;
    property CaretChar: Char read GetCaretChar;

    procedure WndProc(var Msg: TLMessage); override;

    procedure CommandProcessor(Command: TSynEditorCommand; Char: TUTF8Char; Data: Pointer); override;

    procedure AddCommandProcessor(Handler: TLapeTools_CommandProcessor);
    procedure RemoveCommandProcessor(Handler: TLapeTools_CommandProcessor);

    procedure SelectWord(X, Y: Int32); overload;

    procedure Load(AFilePath: String);
    procedure Save;
    procedure SaveAs(AFilePath: String);

    function GetParser(Parse: Boolean; AddImports: Boolean = True): TLapeTools_ScriptParser;
    function GetExpression(var StartXY, EndXY: TPoint): String; overload;
    function GetExpression: String; overload;
    function GetParameterStart(out StartXY: TPoint): Boolean;
    function GetParameterIndex(StartXY: TPoint): Int32;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math, SynGutter, SynGutterLineOverview, SynEditHighlighter, SynEditStrConst, SynEditMouseCmds;

procedure TLapeTools_Editor.WndProc(var Msg: TLMessage);
var
  Command: TSynEditorCommand;
  Char: TUTF8Char = '';
begin
  inherited WndProc(Msg);

  case Msg.Msg of
    LM_KILLFOCUS:
      begin
        Command := lecFocusLost;
        CommandProcessor(Command, Char, nil);
      end;
    LM_SETFOCUS:
      begin
        Command := lecFocus;
        CommandProcessor(Command, Char, nil);
      end;
    LM_VSCROLL, LM_HSCROLL, LM_MOUSEWHEEL:
      begin
        Command := lecScroll;
        CommandProcessor(Command, Char, nil);
      end;
  end;
end;

function TLapeTools_Editor.Scan(XY: TPoint; StopXY: TPoint; Callback: TLapeTools_ScanCallback; Data: Pointer): TPoint;

  function IsJunk(XY: TPoint): Boolean;
  var
    Token: String;
    Attri: TSynHighlighterAttributes;
  begin
    if GetHighlighterAttriAtRowCol(XY, Token, Attri) then
    begin
      if (Attri.Name = SYNS_AttrComment) or (Attri.Name = SYNS_AttrDirective) or (Attri.Name = SYNS_AttrString) then
        Exit(True);
    end;

    Exit(False);
  end;

var
  Line: String;
  Inside: Int32 = 0;
begin
  Line := Copy(Lines[XY.Y - 1], 1, XY.X);

  while (XY.Y > 0) do
  begin
    XY.X := Length(Line) + 1;

    while (XY.X > 0) do
    begin
      if (XY.X = StopXY.X) and (XY.Y = StopXY.Y) then
        Exit(XY);
      if (not IsJunk(Point(XY.X - 1, XY.Y))) and (not Callback(TextBetweenPoints[Point(XY.X - 1, XY.Y), XY], Inside, Data)) then
        Exit(XY);

      Dec(XY.X);
    end;

    if (not Callback(#10, Inside, Data)) then
      Exit(XY);

    Dec(XY.Y);
    if (XY.Y > 0) then
      Line := Lines[XY.Y - 1];
  end;
end;

function TLapeTools_Editor.Scan(XY: TPoint; Callback: TLapeTools_ScanCallback; Data: Pointer): TPoint;
begin
  Result := Scan(Point(XY.X - 1, XY.Y), Point(0, 0), Callback, Data);
end;

function TLapeTools_Editor.GetChangeStamp: Int64;
begin
  Result := GetTextBuffer.TextChangeStamp;
end;

function TLapeTools_Editor.GetScript: String;
begin
  Result := Text;
end;

function TLapeTools_Editor.GetCaret: Int32;
begin
  Result := SelStart;
end;

function TLapeTools_Editor.GetCaretChar: Char;
begin
  if (TextBetweenPoints[Point(CaretX - 1, CaretY), CaretXY] <> '') then
    Result := TextBetweenPoints[Point(CaretX - 1, CaretY), CaretXY][1]
  else
    Result := #0;
end;

procedure TLapeTools_Editor.DoMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := (FOnShowDeclaration <> nil) and (GetExpression() <> '');
end;

procedure TLapeTools_Editor.DoClickLink(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Declaration: TDeclaration;
  Expression: String;
begin
  Expression := GetExpression();

  with GetParser(True) do
  try
    if (Pos('.', Expression) > 0) then
      Declaration := ParseExpression(Expression, False)
    else
      Declaration := Find(Expression);

    if (Declaration <> nil) then
      FOnShowDeclaration(Declaration.DocPos.Line, Declaration.DocPos.Col, Declaration.DocPos.FileName);
  finally
    Free();
  end;
end;

procedure TLapeTools_Editor.DoCaretChange(Sender: TObject);
var
  Command: TSynEditorCommand = lecCaretChange;
  Char: TUTF8Char = '';
begin
  CommandProcessor(Command, Char, nil);
end;

procedure TLapeTools_Editor.CommandProcessor(Command: TSynEditorCommand; Char: TUTF8Char; Data: Pointer);

  function InJunk: Boolean;
  var
    Token: String;
    Attri: TSynHighlighterAttributes;
  begin
    if GetHighlighterAttriAtRowCol(Point(CaretX - 1, CaretY), Token, Attri) then
    begin
      if (Attri.Name = SYNS_AttrComment) or (Attri.Name = SYNS_AttrString) or
         (Attri.Name = SYNS_AttrDirective) or ((Attri.Name = SYNS_AttrNumber) and (Char = '.')) then
          Exit(True);
    end;

    Exit(False);
  end;

var
  i: Int32;
begin
  if (Command < ecUserFirst) then
    inherited CommandProcessor(Command, Char, Data);

  if (Command = ecChar) then
    case Char of
      '.': Command := lecAutoComplete;
      '(': Command := lecParameterHint;
    end;

  if ((Command = lecAutoComplete) or (Command = lecParameterHint)) and InJunk() then
    Exit;

  for i := 0 to High(FCommandProcessors) do
    if (FCommandProcessors[i] <> nil) then
      FCommandProcessors[i](Command, Char);
end;

procedure TLapeTools_Editor.AddCommandProcessor(Handler: TLapeTools_CommandProcessor);
begin
  SetLength(FCommandProcessors, Length(FCommandProcessors) + 1);
  FCommandProcessors[High(FCommandProcessors)] := Handler;
end;

procedure TLapeTools_Editor.RemoveCommandProcessor(Handler: TLapeTools_CommandProcessor);
var
  i: Int32;
begin
  for i := 0 to High(FCommandProcessors) do
    if (FCommandProcessors[i] = Handler) then
      FCommandProcessors[i] := nil;
end;

procedure TLapeTools_Editor.Load(AFilePath: String);
begin
  FFilePath := ExpandFileName(AFilePath);

  Lines.LoadFromFile(FFilePath);

  MarkTextAsSaved();
end;

procedure TLapeTools_Editor.Save;
begin
  Lines.SaveToFile(FFilePath);

  MarkTextAsSaved();
end;

procedure TLapeTools_Editor.SaveAs(AFilePath: String);
begin
  Lines.SaveToFile(AFilePath);

  MarkTextAsSaved();
end;

function TLapeTools_Editor.GetParser(Parse: Boolean; AddImports: Boolean): TLapeTools_ScriptParser;
var
  Search: TSearchRec;
begin
  Result := TLapeTools_ScriptParser.Create(Script, FilePath, Caret);
  Result.OnLibraryDirective := OnLibraryDirective;
  Result.Paths.AddStrings(FPaths);

  if AddImports then
    if FindFirst(FImportsPath + '*.dump', faAnyFile, Search) = 0 then
    begin
      repeat
        Result.HandleDirective(Result.Tokenizer, 'include_once', FImportsPath + Search.Name);
      until FindNext(Search) <> 0;

      FindClose(Search);
    end;

  if Parse then
    Result.Parse();
end;

procedure TLapeTools_Editor.SelectWord(X, Y: Int32);
begin
  if CanFocus() then
    SetFocus();
  if (Y < TopLine) or (Y > TopLine + LinesInWindow) then
    TopLine := Y;

  CaretX := X;
  CaretY := Y;

  if (X > 0) and (Y > 0) then
    SelectWord();
end;

function __GetExpression(Char: String; var Inside: Int32; Data: Pointer): Boolean;
const
  StringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z', '.', '(', '[', '^'];
begin
  if (Char = '') then
    Exit(False);

  case Char[1] of
    ']': Inside := Inside + 1;
    '[': Inside := Inside - 1;
    ')': Inside := Inside + 1;
    '(': Inside := Inside - 1;
  end;

  if (Inside < 0) then
    Exit(False);
  if (Inside > 0) then
    Exit(True);

  Result := Char[1] in StringChars;
end;

function __GetExpressionEx(Char: String; var Inside: Int32; Data: Pointer): Boolean;
begin
  Result := not (Char = '.');
end;

function TLapeTools_Editor.GetExpression(var StartXY, EndXY: TPoint): String;
begin
  Result := '';

  if (TextBetweenPoints[Point(StartXY.X - 1, StartXY.Y), StartXY] <> #32) then
  begin
    while (EndXY.X > 0) and (EndXY.X <= Length(Lines[EndXY.Y - 1])) and (Lines[EndXY.Y - 1][EndXY.X] in IdentChars) do
      Inc(EndXY.X);

    StartXY := Scan(StartXY, @__GetExpression);

    Result := TextBetweenPoints[StartXY, EndXY];

    if (Pos('.', Result) > 0) then
      StartXY := Scan(EndXY, @__GetExpressionEx);
  end;
end;

function TLapeTools_Editor.GetExpression: String;
var
  StartXY, EndXY: TPoint;
begin
  StartXY := CaretXY;
  EndXY := CaretXY;

  Result := GetExpression(StartXY, EndXY);
end;

function __GetParameterStart(Char: String; var Inside: Int32; Data: Pointer): Boolean;
begin
  case Char of
    ')':
      Inside := Inside + 1;
    '(':
      begin
        if (Inside = 0) then
          Exit(False);

        Inside := Inside - 1;
      end;
    ';':
      Exit(False);
  end;

  Exit(True);
end;

function TLapeTools_Editor.GetParameterStart(out StartXY: TPoint): Boolean;
begin
  StartXY := Scan(Point(CaretX - 1, CaretY), Point(0, Max(0, CaretY - 5)), @__GetParameterStart, nil);

  if (TextBetweenPoints[StartXY, Point(StartXY.X - 1, StartXY.Y)] = '(') then
  begin
    StartXY.X := StartXY.X - 1;

    Exit(True);
  end;
end;

function __GetParameterIndex(Char: String; var Inside: Int32; Data: Pointer): Boolean;
begin
  case Char of
    ')':
      Inside := Inside + 1;
    '(':
      Inside := Inside - 1;
    ']':
      Inside := Inside + 1;
    '[':
      if (Inside = 0) then
        PInt32(Data)^ := 0
      else
        Inside := Inside - 1;
    ',':
      if (Inside = 0) then
        PInt32(Data)^ := PInt32(Data)^ + 1;
  end;

  Exit(True);
end;

function TLapeTools_Editor.GetParameterIndex(StartXY: TPoint): Int32;
begin
  Result := 0;

  Scan(Point(CaretX - 1, CaretY), StartXY, @__GetParameterIndex, @Result);
end;

constructor TLapeTools_Editor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MouseOptions := [emShowCtrlMouseLinks, emCtrlWheelZoom];

  Font.Name := 'Consolas';
  Font.Quality := fqDefault;

  OnMouseLink := @DoMouseLink;
  OnClickLink := @DoClickLink;

  with GetCaretObj() do
    AddChangeHandler(@DoCaretChange);

  with KeyStrokes.Add() do
  begin
    Key := VK_ESCAPE;
    Shift := [];
    Command := lecEscape;
  end;

  with KeyStrokes.Add() do
  begin
    Key := VK_SPACE;
    Shift := [ssCtrl];
    Command := lecAutoComplete;
  end;

  with KeyStrokes.Add() do
  begin
    Key := VK_SPACE;
    Shift := [ssShift, ssCtrl];
    Command := lecParameterHint;
  end;

  TSynGutterSeparator.Create(RightGutter.Parts).Width := 1;
  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    TSynGutterLOvProviderCurrentPage.Create(Providers).Priority := 1;
    TSynGutterLOvProviderModifiedLines.Create(Providers).Priority := 2;
  end;

  FPaths := TStringList.Create();
end;

destructor TLapeTools_Editor.Destroy;
begin
  FPaths.Free();

  inherited Destroy();
end;

end.

