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
  lecPaint = ecUserFirst + 6;

type
  TLapeTools_ShowDeclaration = procedure(Line, Column: Int32; FilePath: String) of object;
  TLapeTools_CommandProcessor = procedure(var Command: TSynEditorCommand; Char: TUTF8Char) of object;
  TLapeTools_CommandProcessors = array of TLapeTools_CommandProcessor;

  TLapeTools_Editor = class(TSynEdit)
  protected
    FFilePath: String;
    FPaths: TStringList;
    FOnShowDeclaration: TLapeTools_ShowDeclaration;
    FCommandProcessors: TLapeTools_CommandProcessors;

    function GetChangeStamp: Int64;
    function GetScript: String;
    function GetCaret: Int32;

    procedure DoMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoClickLink(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoCaretChange(Sender: TObject);
    procedure DoPaint(Sender: TObject; ACanvas: TCanvas);
  public
    property OnShowDeclaration: TLapeTools_ShowDeclaration read FOnShowDeclaration write FOnShowDeclaration;

    property ChangeStamp: Int64 read GetChangeStamp;
    property FilePath: String read FFilePath;
    property Paths: TStringList read FPaths;
    property Script: String read GetScript;
    property Caret: Int32 read GetCaret;

    procedure WndProc(var Msg: TLMessage); override;

    procedure CommandProcessor(Command: TSynEditorCommand; Char: TUTF8Char; Data: Pointer); override;

    procedure AddCommandProcessor(Handler: TLapeTools_CommandProcessor);
    procedure RemoveCommandProcessor(Handler: TLapeTools_CommandProcessor);

    procedure SelectWord(X, Y: Int32); overload;

    procedure Load(AFilePath: String);
    procedure Save;
    procedure SaveAs(AFilePath: String);

    function GetParser(Parse: Boolean): TLapeTools_ScriptParser;
    function GetExpression(var StartPos, EndPos: TPoint): String; overload;
    function GetExpression: String; overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SynGutter, SynGutterLineOverview, SynEditHighlighter, SynEditStrConst, SynEditMouseCmds;

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
  end;
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

procedure TLapeTools_Editor.DoPaint(Sender: TObject; ACanvas: TCanvas);
var
  Command: TSynEditorCommand = lecPaint;
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
         (Attri.Name = SYNS_AttrDirective) or (Attri.Name = SYNS_AttrNumber) then
        Exit(True);
    end;

    Exit(False);
  end;

var
  i: Int32;
begin
  if (Command < ecUserFirst) then
    inherited CommandProcessor(Command, Char, Data);

  if (Command = ecChar) and (Char = '.') then
    Command := lecAutoComplete;
  if (Command = lecAutoComplete) and InJunk() then
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

function TLapeTools_Editor.GetParser(Parse: Boolean): TLapeTools_ScriptParser;
begin
  Result := TLapeTools_ScriptParser.Create(Script, FilePath, Caret);
  Result.Paths.AddStrings(FPaths);
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

  SelectWord();
end;

function TLapeTools_Editor.GetExpression(var StartPos, EndPos: TPoint): String;

  function IsComment(Position: TPoint): Boolean;
  var
    Token: String;
    Attri: TSynHighlighterAttributes;
  begin
    Result := GetHighlighterAttriAtRowCol(Position, Token, Attri) and (Attri.Name = SYNS_AttrComment);
  end;

  procedure GetStart;
  var
    Line: String;
    Lock: Int32;
  begin
    Lock := 0;

    while (StartPos.Y > 0) do
    begin
      Line := Lines[StartPos.Y - 1];
      StartPos.X := Length(Line) + 1;

      while (StartPos.X > 1) do
      begin
        if (not IsComment(StartPos)) then
          case Line[StartPos.X - 1] of
            '[', '(':
              Inc(Lock);
            ']', ')':
              Dec(Lock);
            ' ', #9:
              if (Lock = 0) then
                Exit;
          end;

        Dec(StartPos.X);
      end;

      if (Lock = 0) then
        Exit;

      Dec(StartPos.Y);
    end;
  end;

  procedure GetEnd;
  var
    Line: String;
  begin
    Line := Lines[EndPos.Y - 1];
    while (EndPos.X <= Length(Line)) and (Line[EndPos.X] in ['_', '0'..'9', 'A'..'Z', 'a'..'z']) do
      Inc(EndPos.X);

    StartPos := EndPos;
  end;

begin
  GetEnd();
  if (Length(Lines[EndPos.Y - 1]) > 0) and (EndPos.X <= Length(Lines[EndPos.Y - 1]) + 1) then
    GetStart();

  Result := TextBetweenPoints[StartPos, EndPos];

  if (Pos('.', Result) > 0) then
  begin
    StartPos := EndPos;

    while (StartPos.X > 1) and (Lines[StartPos.Y - 1][StartPos.X - 1] <> '.') do
      Dec(StartPos.X);
  end;
end;

function TLapeTools_Editor.GetExpression: String;
var
  StartPos, EndPos: TPoint;
begin
  StartPos := CaretXY;
  EndPos := CaretXY;

  Result := GetExpression(StartPos, EndPos);
end;

constructor TLapeTools_Editor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MouseOptions := [emShowCtrlMouseLinks, emCtrlWheelZoom];

  Font.Name := 'Consolas';
  Font.Quality := fqDefault;

  OnPaint := @DoPaint;
  OnMouseLink := @DoMouseLink;
  OnClickLink := @DoClickLink;

  with GetCaretObj() do
    AddChangeHandler(@DoCaretChange);

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

