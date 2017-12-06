unit lpt_parameterhint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, SynEditKeyCmds, LCLType,
  lpt_editor, lpt_parser;

type
  TLapeTools_ParameterHint = class;

  TLapeTools_ParameterHint_Form = class(THintWindow)
  protected
    FBuffer: TBitmap;
    FParameterHint: TLapeTools_ParameterHint;
    FMethods: TDeclaration_Methods;
    FIndex: Int32;

    procedure TextOut(var X, Y: Int32; AText: String; AColor: Int32 = clBlack; AStyle: TFontStyles = []);

    function DrawMethod(X, Y: Int32; Method: TDeclaration_Method): Int32;
  public
    property ParameterHint: TLapeTools_ParameterHint read FParameterHint write FParameterHint;
    property Index: Int32 read FIndex write FIndex;

    procedure Paint; override;
    procedure Show(X, Y: Int32; Methods: TDeclaration_Methods); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TLapeTools_ParameterHint = class(TComponent)
  protected
    FParser: TLapeTools_Parser;
    FEditor: TLapeTools_Editor;
    FForm: TLapeTools_ParameterHint_Form;
    FStartPos: TPoint;
    FEndPos: TPoint;
    FExpression: String;

    procedure DoApplicationLostFocus(Sender: TObject);
    procedure DoCommandProcessor(var Command: TSynEditorCommand; Char: TUTF8Char);

    procedure SetEditor(Value: TLapeTools_Editor);
  public
    property Editor: TLapeTools_Editor read FEditor write SetEditor;

    procedure Execute(X, Y: Int32);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lpparser;

procedure TLapeTools_ParameterHint_Form.TextOut(var X, Y: Int32; AText: String; AColor: Int32; AStyle: TFontStyles);
begin
  with FBuffer do
  begin
    Canvas.Font.Color := AColor;
    Canvas.Font.Style := AStyle;
    Canvas.TextOut(X, Y, AText);

    X := X + Canvas.TextWidth(AText);
    if (fsBold in AStyle) then
      X := X + 1;
  end;
end;

function TLapeTools_ParameterHint_Form.DrawMethod(X, Y: Int32; Method: TDeclaration_Method): Int32;
type
  TParameterGroups = array of array of TDeclaration_Parameter;

  function GroupParameters: TParameterGroups;
  var
    i: Int32;
    Groups: TParameterGroups absolute Result;
  begin
    SetLength(Groups, 0);

    with Method.Header do
      for i := 0 to Parameters.Count - 1 do
      begin
        if (i = 0) or (Parameters.Get(i).Group <> Parameters.Get(i - 1).Group) then
          SetLength(Groups, Length(Groups) + 1);

        SetLength(Groups[High(Groups)], Length(Groups[High(Groups)]) + 1);
        Groups[High(Groups)][High(Groups[High(Groups)])] := Parameters.Get(i);
      end;
  end;

var
  Parameters: TParameterGroups;
  i, j, h: Int32;
  Style: TFontStyles;
begin
  Parameters := GroupParameters();

  X := BorderWidth;

  TextOut(X, Y, Method.Header.Name.Text, clBlack);
  TextOut(X, Y, '(', clBlack);

  for i := 0 to High(Parameters) do
  begin
    h := High(Parameters[i]);

    if (Index >= i) and (Index <= i + h) then
      Style := [fsBold]
    else
      Style := [];

    case Parameters[i][0].Modifier of
      pmConst: TextOut(X, Y, 'const ', clBlack, Style);
      pmConstRef: TextOut(X, Y, 'const ', clBlack, Style);
      pmVar: TextOut(X, Y, 'var ', clBlack, Style);
      pmOut: TextOut(X, Y, 'out ', clBlack, Style);
    end;

    for j := 0 to h do
    begin
      if (i + j = Index) then
        TextOut(X, Y, Parameters[i][j].Name.Text, clBlack, [fsBold])
      else
        TextOut(X, Y, Parameters[i][j].Name.Text);

      if (j < h) then
        TextOut(X, Y, ', ');
    end;

    if (Parameters[i][0].VarType <> nil) then
    begin
      TextOut(X, Y, ': ');
      TextOut(X, Y, Parameters[i][0].VarType.Text, clBlack, Style)
    end;

    if (Parameters[i][0].Value <> nil) then
    begin
      TextOut(X, Y, ' = ');
      TextOut(X, Y, Parameters[i][0].Value.Text, clBlack, Style);
    end;

    if (i < High(Parameters)) then
      TextOut(X, Y, '; ');
  end;

  TextOut(X, Y, ')');

  if (Method.Header.MethodType in [mtFunction, mtFunctionOfObject]) then
    TextOut(X, Y, ': ' + Method.Header.Result.Text);

  Result := X + BorderWidth;
end;

procedure TLapeTools_ParameterHint_Form.Paint;
var
  Y, i: Int32;
begin
  FBuffer.SetSize(Width, Height);

  with FBuffer do
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := $F0F0F0;
    Canvas.Rectangle(ClientRect);
  end;

  for i := 0 to High(FMethods) do
    DrawMethod(BorderWidth, ((i * FParameterHint.Editor.LineHeight) + BorderWidth) - 1, FMethods[i]);

  Canvas.Draw(0, 0, FBuffer);
end;

procedure TLapeTools_ParameterHint_Form.Show(X, Y: Int32; Methods: TDeclaration_Methods);
var
  i: Int32;
begin
  FBuffer.Canvas.Font := Font;
  FMethods := Methods;
  FIndex := -1;

  if (Length(FMethods) > 0) then
  begin
    Height := (Length(FMethods) * FParameterHint.Editor.LineHeight) + (BorderWidth * 2);
    Width := DrawMethod(0, 0, FMethods[0]) + 10;

    for i := 1 to High(FMethods) do
      if (DrawMethod(0, 0, FMethods[i]) > Width) then
        Width := DrawMethod(0, 0, FMethods[i]) + 10;

    Left := X;
    Top := Y - Height;

    Show();
  end else
    Hide();
end;

constructor TLapeTools_ParameterHint_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBuffer := TBitmap.Create();
end;

destructor TLapeTools_ParameterHint_Form.Destroy;
begin
  FBuffer.Free();

  inherited Destroy();
end;

procedure TLapeTools_ParameterHint.DoApplicationLostFocus(Sender: TObject);
begin
  if FForm.Showing then
    FForm.Hide();
end;

procedure TLapeTools_ParameterHint.DoCommandProcessor(var Command: TSynEditorCommand; Char: TUTF8Char);

  function GetParameterIndex: Int32;
  var
    Tokenizer: TLapeTokenizerString;
    Lock: Int32 = 0;
  begin
    Result := 0;

    if (FEditor.TextBetweenPoints[FEndPos, Point(FEndPos.X + 1, FEndPos.Y)] = '(') and
       (FEditor.TextBetweenPoints[Point(Editor.CaretX - 1, Editor.CaretY), Editor.CaretXY] <> ')') then
    begin
      Tokenizer := TLapeTokenizerString.Create(FEditor.TextBetweenPoints[Point(FEndPos.X + 1, FEndPos.Y), Editor.CaretXY]);

      try
        Tokenizer.NextNoJunk();

        while (Tokenizer.Tok <> tk_NULL) do
        begin
          case Tokenizer.Tok of
            tk_sym_BracketOpen, tk_sym_ParenthesisOpen:
              Lock := Lock + 1;
            tk_sym_BracketClose, tk_sym_ParenthesisClose:
              Lock := Lock - 1;
            tk_sym_Comma:
              if (Lock = 0) then
                Result := Result + 1;
          end;

          Tokenizer.NextNoJunk();
        end;
      except
        { nothing }
      end;

      Tokenizer.Free();
    end else
      Result := -1;
  end;

begin
  case Command of
    lecParameterHint:
      begin
        if FForm.Showing then
          FForm.Hide()
        else
        begin
          if (FParser <> nil) then
            FParser.Free();

          FParser := FEditor.GetParser(True);
          FStartPos := FEditor.GetParameterStart();
          FStartPos.X := FStartPos.X - 1;
          FEndPos := FStartPos;
          FExpression := FEditor.GetExpression(FStartPos, FEndPos);

          with FEditor.ClientToScreen(FEditor.RowColumnToPixels(FStartPos)) do
            Execute(X - FForm.BorderWidth, Y);

          FForm.Index := GetParameterIndex();
          FForm.Paint();
        end;
      end;

    lecFocusLost:
      if (GetParentForm(FEditor).ActiveControl <> FEditor) then
        FForm.Hide();

    lecScroll:
      if FForm.Showing then
      begin
        if (FStartPos.Y >= FEditor.TopLine) and (FStartPos.Y < FEditor.TopLine + FEditor.LinesInWindow) then
        begin
          with FEditor.ClientToScreen(FEditor.RowColumnToPixels(FStartPos)) do
          begin
            FForm.Left := X - FForm.BorderWidth;
            FForm.Top := Y;
          end;
        end else
          FForm.Hide();
      end;

    lecCaretChange, ecChar:
      begin
        FForm.Index := GetParameterIndex();

        if (FForm.Index >= 0) then
          FForm.Paint()
        else
          FForm.Hide();
      end;
  end;
end;

procedure TLapeTools_ParameterHint.SetEditor(Value: TLapeTools_Editor);
begin
  FEditor := Value;
  FEditor.AddCommandProcessor(@DoCommandProcessor);
end;

procedure TLapeTools_ParameterHint.Execute(X, Y: Int32);
var
  Declaration: TDeclaration;
begin
  if (not FForm.Font.IsEqual(FEditor.Font)) then
    FForm.Font := FEditor.Font;

  if (Pos('.', FExpression) > 0) then
    Declaration := FParser.ParseExpression(FExpression)
  else
    Declaration := FParser.Find(FExpression);

  if (Declaration <> nil) and (Declaration is TDeclaration_Method) then
  begin
    with Declaration as TDeclaration_Method do
      case Header.MethodType of
        mtProcedure, mtFunction:
          FForm.Show(X, Y, FParser.Map.GetMethods(Header.Name.Text));
        mtProcedureOfObject, mtFunctionOfObject:
          FForm.Show(X, Y, FParser.Map.GetMethods(Header.ObjectName.Text + '.' + Header.Name.Text));
      end;
  end else
    FForm.Hide();

  if FEditor.CanFocus() then
    FEditor.SetFocus();
end;

constructor TLapeTools_ParameterHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := TLapeTools_ParameterHint_Form.Create(Self);
  FForm.ShowInTaskBar := stNever;
  FForm.BorderWidth := 2;
  FForm.ParameterHint := Self;

  Application.AddOnDeactivateHandler(@DoApplicationLostFocus);
end;

destructor TLapeTools_ParameterHint.Destroy;
begin
  if (FParser <> nil) then
    FParser.Free();

  Application.RemoveOnDeactivateHandler(@DoApplicationLostFocus);

  inherited Destroy;
end;

end.

