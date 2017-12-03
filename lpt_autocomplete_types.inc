{%MainUnit lpt_autocomplete.pas}

type
  TLapeTools_AutoComplete_Item = class(TTreeNode)
  protected
    FAutoComplete: TLapeTools_AutoComplete;

    function DisplayLeft(Column: Boolean): Int32;
    function DisplayTop: Int32;

    procedure TextOut(var X, Y: Int32; AText: String; AColor: Int32; AStyle: TFontStyles = []); overload;
    procedure TextOut(var X, Y: Int32; AText: String); overload;
  public
    procedure DrawPopup; virtual;
    procedure DrawBackground; virtual;
    procedure DrawColumn; virtual;
    procedure DrawText; virtual;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete); reintroduce;
  end;

function TLapeTools_AutoComplete_Item.DisplayLeft(Column: Boolean): Int32;
begin
  with FAutoComplete.Tree do
  begin
    Result := BorderWidth - ScrolledLeft + 2;

    if Column then
      Result := Result + Canvas.TextWidth('procedure ');
  end;
end;

function TLapeTools_AutoComplete_Item.DisplayTop: Int32;
begin
  with FAutoComplete.Tree do
    Result := Self.Top - ScrolledTop + BorderWidth;
end;

procedure TLapeTools_AutoComplete_Item.TextOut(var X, Y: Int32; AText: String; AColor: Int32; AStyle: TFontStyles);
begin
  with FAutoComplete.Tree do
  begin
    Canvas.Font.Color := AColor;
    Canvas.Font.Style := AStyle;
    Canvas.TextOut(X, Y, AText);

    X := X + Canvas.TextWidth(AText);
    if (fsBold in AStyle) then
      X := X + 1;
  end;
end;

procedure TLapeTools_AutoComplete_Item.TextOut(var X, Y: Int32; AText: String);
var
  Highlighter: TSynCustomHighlighter;
  TokStart: PChar;
  TokLen: Int32;
  TokString: String;
begin
  Highlighter := FAutoComplete.Editor.Highlighter;
  Highlighter.ResetRange();
  Highlighter.SetLine(AText, 0);

  while (not Highlighter.GetEol()) do
  begin
    Highlighter.GetTokenEx(TokStart, TokLen);

    if (TokLen > 0) then
    begin
      SetLength(TokString, TokLen);

      Move(TokStart^, TokString[1], TokLen);

      with Highlighter.GetTokenAttribute(), FAutoComplete.Tree do
      begin
        if (Foreground = clNone) then
          Canvas.Font.Color := clBlack
        else
          Canvas.Font.Color := ColorToRGB(Foreground);

        Canvas.Font.Style := [];
        Canvas.TextOut(X, Y, TokString);

        X := X + Canvas.TextWidth(TokString);
      end;
    end;

    Highlighter.Next();
  end;
end;

procedure TLapeTools_AutoComplete_Item.DrawPopup;
begin
  with FAutoComplete.Popup do
  begin
    Header := '';
    Description := '';
    Declaration := nil;
  end;
end;

procedure TLapeTools_AutoComplete_Item.DrawBackground;
begin
  with FAutoComplete.Tree do
  begin
    if Self.Selected then
      Canvas.Brush.Color := $CBC0B4
    else
    if Odd(Index) then
      Canvas.Brush.Color := $F0F0F0
    else
      Canvas.Brush.Color := clWhite;

    Canvas.FillRect(DisplayRect(False));
  end;
end;

procedure TLapeTools_AutoComplete_Item.DrawColumn;
begin
  { nothing }
end;

procedure TLapeTools_AutoComplete_Item.DrawText;
begin
  { nothing }
end;

constructor TLapeTools_AutoComplete_Item.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete);
begin
  inherited Create(AOwner);

  FAutoComplete := AutoComplete;
end;

type
  TLapeTools_AutoComplete_Type = class(TLapeTools_AutoComplete_Item)
  protected
    FType: TDeclaration_Type;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;
    procedure DrawColumn; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Type.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := 'type ' + FType.Name.Text + ' = ' + SingleLine(FType.Text);
  FAutoComplete.Popup.Declaration := FType;
end;

procedure TLapeTools_AutoComplete_Type.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FType.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ' = ' + SingleLine(FType.Text));
end;

procedure TLapeTools_AutoComplete_Type.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'type', clMaroon);
end;

constructor TLapeTools_AutoComplete_Type.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type);
begin
  inherited Create(AOwner, AutoComplete);

  FType := Declaration;
end;

type
  TLapeTools_AutoComplete_Record = class(TLapeTools_AutoComplete_Type)
  protected
    FRecord: TDeclaration_Type_Record;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Record); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Record.DrawPopup;
var
  Header: String;
  i: Int32;
begin
  inherited DrawPopup();

  Header := 'type' + LineEnding;
  Header := Header + '  ' + FRecord.Name.Text + ' = record';
  if (FRecord.Parent <> nil) then
    Header := Header + '(' + FRecord.Parent.Text + ')';
  Header := Header + LineEnding;

  for i := 0 to FRecord.Fields.Count - 1 do
    with FRecord.Fields.Get(i) as TDeclaration_Field do
    begin
      Header := Header + '    ' + Name.Text;
      if (VarType <> nil) then
        Header := Header + ': ' + VarType.Text + ';';
      Header := Header + LineEnding;
    end;

  Header := Header + '  ' + 'end';

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FRecord;
end;

procedure TLapeTools_AutoComplete_Record.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  with FAutoComplete.Tree do
  begin
    TextOut(X, Y, FRecord.Name.Text, clBlack, [fsBold]);
    TextOut(X, Y, ' = ', clRed);
    TextOut(X, Y, 'record', clBlack);

    if (FRecord.Parent <> nil) then
    begin
      TextOut(X, Y, '(', clRed);
      TextOut(X, Y, FRecord.Parent.Text, clBlack);
      TextOut(X, Y, ')', clRed);
    end;
  end;
end;

constructor TLapeTools_AutoComplete_Record.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Record);
begin
  inherited Create(AOwner, AutoComplete, Declaration);

  FRecord := Declaration;
end;

type
  TLapeTools_AutoComplete_Enum = class(TLapeTools_AutoComplete_Item)
  protected
    FEnum: TDeclaration_EnumElement;
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_EnumElement); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Enum.DrawPopup;
var
  Header: String;
begin
  inherited DrawPopup();

  Header := 'enum ' + FEnum.Name.Text + ': ' + FEnum.Enum.Name.Text;
  if (FEnum.Value <> nil) then
    Header := Header + '(' + FEnum.Value.Text + ')';

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FEnum;
end;

procedure TLapeTools_AutoComplete_Enum.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'enum', clOlive);
end;

procedure TLapeTools_AutoComplete_Enum.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FEnum.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ': ' + FEnum.Enum.Name.Text);
  if (FEnum.Value <> nil) then
    TextOut(X, Y, '(' + FEnum.Value.Text + ')');
end;

constructor TLapeTools_AutoComplete_Enum.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_EnumElement);
begin
  inherited Create(AOwner, AutoComplete);

  FEnum := Declaration;
end;

type
  TLapeTools_AutoComplete_Method = class(TLapeTools_AutoComplete_Item)
  protected
    FMethod: TDeclaration_Method;
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Method); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Method.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := SingleLine(FMethod.Header.Text);
  FAutoComplete.Popup.Declaration := FMethod;
  if (FMethod.Description <> '') then
    FAutoComplete.Popup.Description := FMethod.Description;
end;

procedure TLapeTools_AutoComplete_Method.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  with FAutoComplete.Tree do
  begin
    if (FMethod.Header.MethodType in [mtFunction, mtFunctionOfObject]) then
      TextOut(X, Y, 'function', clTeal)
    else
      TextOut(X, Y, 'procedure', clNavy);
  end;
end;

procedure TLapeTools_AutoComplete_Method.DrawText;
type
  TParameterGroups = array of array of TDeclaration_Parameter;

  function GroupParameters: TParameterGroups;
  var
    i: Int32;
    Groups: TParameterGroups absolute Result;
  begin
    SetLength(Groups, 0);

    with FMethod.Header do
      for i := 0 to Parameters.Count - 1 do
      begin
        if (i = 0) or (Parameters.Get(i).Group <> Parameters.Get(i - 1).Group) then
          SetLength(Groups, Length(Groups) + 1);

        SetLength(Groups[High(Groups)], Length(Groups[High(Groups)]) + 1);
        Groups[High(Groups)][High(Groups[High(Groups)])] := Parameters.Get(i);
      end;
  end;

var
  X, Y, i, j, h: Int32;
  Parameters: TParameterGroups;
  Header: String = '';
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  Parameters := GroupParameters();

  if (Length(Parameters) > 0) then
  begin
    Header := Header + '(';

    for i := 0 to High(Parameters) do
    begin
      case Parameters[i][0].Modifier of
        pmOut: Header := Header + 'out ';
        pmVar: Header := Header + 'var ';
        pmConst: Header := Header + 'const ';
        pmConstRef: Header := Header + 'constref ';
      end;

      h := High(Parameters[i]);
      for j := 0 to h do
      begin
         Header := Header + Parameters[i][j].Name.Text;

        if (j < h) then
          Header := Header +  ', ';

        if (j = h) then
        begin
          if (Parameters[i][j].VarType <> nil) then
          begin
            Header := Header +  ': ';
            Header := Header +  Parameters[i][j].VarType.Text;
          end;

          if (i < High(Parameters)) then
            Header := Header + '; ';
        end;
      end;
    end;

    Header := Header + ')';
  end;

  if (FMethod.Header.Result <> nil) then
  begin
    Header := Header + ': ';
    Header := Header +  FMethod.Header.Result.Text;
  end;

  TextOut(X, Y, FMethod.Header.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, Header);
end;

constructor TLapeTools_AutoComplete_Method.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Method);
begin
  inherited Create(AOwner, AutoComplete);

  FMethod := Declaration;
end;

type
  TLapeTools_AutoComplete_Variable = class(TLapeTools_AutoComplete_Item)
  protected
    FVariable: TDeclaration_Variable;
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Variable); reintroduce;
  end;

constructor TLapeTools_AutoComplete_Variable.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Variable);
begin
  inherited Create(AOwner, AutoComplete);

  FVariable := Declaration;
end;

procedure TLapeTools_AutoComplete_Variable.DrawPopup;
var
  Header: String;
begin
  inherited DrawPopup();

  Header := 'var ' + FVariable.Name.Text;
  if (FVariable.VarType <> nil) then
    Header := Header + ': ' + FVariable.VarType.Text;
  if (FVariable.Value <> nil) then
    Header := Header + ' = ' + SingleLine(FVariable.Value.Text);

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FVariable;
end;

procedure TLapeTools_AutoComplete_Variable.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'variable', clGreen);
end;

procedure TLapeTools_AutoComplete_Variable.DrawText;
var
  X, Y: Int32;
  Header: String = '';
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  if (FVariable.VarType <> nil) then
    Header := Header + ': ' + FVariable.VarType.Text;

  if (FVariable.Value <> nil) then
    Header := Header + ' = ' + SingleLine(FVariable.Value.Text);

  TextOut(X, Y, FVariable.Name.Text, clBlack, [fsBold]);
  TExtOut(X, Y, Header);
end;

type
  TLapeTools_AutoComplete_Constant = class(TLapeTools_AutoComplete_Variable)
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
  end;

procedure TLapeTools_AutoComplete_Constant.DrawPopup;
var
  Header: String;
begin
  inherited DrawPopup();

  Header := 'const ' + FVariable.Name.Text;
  if (FVariable.VarType <> nil) then
    Header := Header + ': ' + FVariable.VarType.Text;
  if (FVariable.Value <> nil) then
    Header := Header + ' = ' + SingleLine(FVariable.Value.Text);

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FVariable;
end;

procedure TLapeTools_AutoComplete_Constant.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'constant', clPurple);
end;

type
  TLapeTools_AutoComplete_Field = class(TLapeTools_AutoComplete_Variable)
  public
    procedure DrawColumn; override;
  end;

procedure TLapeTools_AutoComplete_Field.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'field', clBlue);
end;

type
  TLapeTools_AutoComplete_Parameter = class(TLapeTools_AutoComplete_Variable)
  public
    procedure DrawColumn; override;
  end;

procedure TLapeTools_AutoComplete_Parameter.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'parameter', $000099);
end;

type
  TLapeTools_AutoComplete_Label = class(TLapeTools_AutoComplete_Item)
  protected
    FLabel: TDeclaration_Label;
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Label); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Label.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := 'label ' + FLabel.Name.Text;
  FAutoComplete.Popup.Declaration := FLabel;
end;

procedure TLapeTools_AutoComplete_Label.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'label', clFuchsia);
end;

procedure TLapeTools_AutoComplete_Label.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FLabel.Name.Text, clBlack, [fsBold]);
end;

constructor TLapeTools_AutoComplete_Label.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Label);
begin
  inherited Create(AOwner, AutoComplete);

  FLabel := Declaration;
end;

type
  TLapeTools_AutoComplete_Self = class(TLapeTools_AutoComplete_Item)
  protected
    FType: TDeclaration_Type;
  public
    procedure DrawColumn; override;
    procedure DrawText; override;
    procedure DrawPopup; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Self.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'variable', clGreen);
end;

procedure TLapeTools_AutoComplete_Self.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, 'Self', clBlack, [fsBold]);

  if (FType.Name <> nil) then
  begin
    TextOut(X, Y, ': ', clRed);
    TextOut(X, Y, FType.Name.Text, clBlack);
  end;
end;

procedure TLapeTools_AutoComplete_Self.DrawPopup;
var
  Header: String;
begin
  inherited DrawPopup();

  Header := 'var Self';
  if (FType.Name <> nil) then
    Header := Header + ': ' + FType.Name.Text;

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FType;
end;

constructor TLapeTools_AutoComplete_Self.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type);
begin
  inherited Create(AOwner, AutoComplete);

  FType := Declaration;
end;

type
  TLapeTools_AutoComplete_Result = class(TLapeTools_AutoComplete_Item)
  protected
    FResult: TDeclaration_Type;
  public
    procedure DrawColumn; override;
    procedure DrawText; override;
    procedure DrawPopup; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Result.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'variable', clGreen);
end;

procedure TLapeTools_AutoComplete_Result.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, 'Result', clBlack, [fsBold]);
  TextOut(X, Y, ': ', clRed);
  TextOut(X, Y, FResult.Text, clBlack);
end;

procedure TLapeTools_AutoComplete_Result.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := 'var Result: ' + FResult.Text;
  FAutoComplete.Popup.Declaration := FResult;
end;

constructor TLapeTools_AutoComplete_Result.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type);
begin
  inherited Create(AOwner, AutoComplete);

  FResult := Declaration;
end;

