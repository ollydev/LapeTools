{%MainUnit lpt_autocomplete.pas}

type
  TLapeTools_AutoComplete_Item = class(TTreeNode)
  protected
    FAutoComplete: TLapeTools_AutoComplete;
    FName: String;

    function DisplayLeft(Column: Boolean): Int32;
    function DisplayTop: Int32;

    procedure TextOut(var X, Y: Int32; AText: String; AColor: Int32; AStyle: TFontStyles = []);
  public
    property Name: String read FName;

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

procedure TLapeTools_AutoComplete_Item.DrawPopup;
begin
  with FAutoComplete.Popup do
  begin
    Header := '';
    Description := '';
    Declaration := nil;

    if Odd(Index) then
      Color := $F0F0F0
    else
      Color := clWhite;
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
  public
    procedure DrawColumn; override;
  end;

procedure TLapeTools_AutoComplete_Type.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'type', clMaroon);
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
        Header := Header + ': ' + VarType.Text;
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
  inherited Create(AOwner, AutoComplete);

  FRecord := Declaration;
  FName := Declaration.Name.Text;
end;

type
  TLapeTools_AutoComplete_Array = class(TLapeTools_AutoComplete_Type)
  protected
    FArray: TDeclaration_Type_Array;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Array); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Array.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := 'type ' + FArray.Name.Text + ' = ' + FArray.Text;
  FAutoComplete.Popup.Declaration := FArray;
end;

procedure TLapeTools_AutoComplete_Array.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FArray.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ' = ', clRed);
  TextOut(X, Y, FArray.Text, clBlack);
end;

constructor TLapeTools_AutoComplete_Array.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Array);
begin
  inherited Create(AOwner, AutoComplete);

  FArray := Declaration;
  FName := Declaration.Name.Text;
end;

type
  TLapeTools_AutoComplete_Alias = class(TLapeTools_AutoComplete_Type)
  protected
    FAlias: TDeclaration_Type_Alias;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Alias); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Alias.DrawPopup;
begin
  inherited DrawPopup;

  FAutoComplete.Popup.Header := 'type ' + FAlias.Name.Text + ' = ' + FAlias.AliasType.Text;
  FAutoComplete.Popup.Declaration := FAlias;
end;

procedure TLapeTools_AutoComplete_Alias.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FAlias.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ' = ', clRed);
  TextOut(X, Y, FAlias.Text, clBlack);
end;

constructor TLapeTools_AutoComplete_Alias.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Alias);
begin
  inherited Create(AOwner, AutoComplete);

  FAlias := Declaration;
  FName := Declaration.Name.Text;
end;

type
  TLapeTools_AutoComplete_Enum = class(TLapeTools_AutoComplete_Type)
  protected
    FEnum: TDeclaration_Type_Enum;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Enum); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Enum.DrawPopup;
var
  Header: String;
  i: Int32;
begin
  inherited DrawPopup();

  Header := 'type ' + FEnum.Name.Text + ' = ';
  Header := Header + '(';

  for i := 0 to FEnum.Elements.Count - 1 do
  begin
    if (i > 0) then
      Header := Header + ', ';
    with FEnum.Elements.Get(i) as TDeclaration_EnumElement do
      Header := Header + Name.Text;
  end;

  Header := Header + ')';

  FAutoComplete.Popup.Header := Header;
  FAutoComplete.Popup.Declaration := FEnum;
end;

procedure TLapeTools_AutoComplete_Enum.DrawText;
var
  X, Y, i: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FEnum.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ' = (', clRed);

  for i := 0 to FEnum.Elements.Count - 1 do
    with FEnum.Elements.Get(i) as TDeclaration_EnumElement do
    begin
      if (i > 0) then
        TextOut(X, Y, ', ', clRed);

      TextOut(X, Y, Name.Text, clBlack);
    end;

  TextOut(X, Y, ')', clRed);
end;

constructor TLapeTools_AutoComplete_Enum.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Enum);
begin
  inherited Create(AOwner, AutoComplete);

  FEnum := Declaration;
  FName := Declaration.Name.Text;
end;

type
  TLapeTools_AutoComplete_EnumElement = class(TLapeTools_AutoComplete_Item)
  protected
    FElement: TDeclaration_EnumElement;
  public
    procedure DrawPopup; override;
    procedure DrawColumn; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_EnumElement); reintroduce;
  end;

procedure TLapeTools_AutoComplete_EnumElement.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := FElement.Name.Text + ': ' + FElement.Enum.Name.Text;
  FAutoComplete.Popup.Declaration := FElement;
end;

procedure TLapeTools_AutoComplete_EnumElement.DrawColumn;
var
  X, Y: Int32;
begin
  X := DisplayLeft(False);
  Y := DisplayTop();

  TextOut(X, Y, 'enum', clOlive);
end;

procedure TLapeTools_AutoComplete_EnumElement.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FElement.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ': ', clRed);
  TextOut(X, Y, FElement.Enum.Name.Text, clBlack);
end;

constructor TLapeTools_AutoComplete_EnumElement.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_EnumElement);
begin
  inherited Create(AOwner, AutoComplete);

  FElement := Declaration;
  FName := Declaration.Name.Text;
end;

type
  TLapeTools_AutoComplete_Copy = class(TLapeTools_AutoComplete_Type)
  protected
    FCopy: TDeclaration_Type_Copy;
  public
    procedure DrawPopup; override;
    procedure DrawText; override;

    constructor Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Copy); reintroduce;
  end;

procedure TLapeTools_AutoComplete_Copy.DrawPopup;
begin
  inherited DrawPopup();

  FAutoComplete.Popup.Header := 'type ' + FCopy.Name.Text + ' = type ' + FCopy.CopyType.Text;
  FAutoComplete.Popup.Declaration := FCopy;
end;

procedure TLapeTools_AutoComplete_Copy.DrawText;
var
  X, Y: Int32;
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FCopy.Name.Text, clBlack, [fsBold]);
  TextOut(X, Y, ' = ', clRed);
  TextOut(X, Y, 'type ' + FCopy.CopyType.Text, clBlack);
end;

constructor TLapeTools_AutoComplete_Copy.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Type_Copy);
begin
  inherited Create(AOwner, AutoComplete);

  FCopy := Declaration;
  FName := Declaration.Name.Text;
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
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  with FAutoComplete.Tree do
  begin
    TextOut(X, Y, FMethod.Header.Name.Text, clBlack, [fsBold]);

    Parameters := GroupParameters();

    if (Length(Parameters) > 0) then
    begin
      TextOut(X, Y, '(', clRed);

      for i := 0 to High(Parameters) do
      begin
        case Parameters[i][0].Modifier of
          pmOut: TextOut(X, Y, 'out ', clBlack);
          pmVar: TextOut(X, Y, 'var ', clBlack);
          pmConst: TextOut(X, Y, 'const ', clBlack);
          pmConstRef: TextOut(X, Y, 'constref ', clBlack);
        end;

        h := High(Parameters[i]);
        for j := 0 to h do
        begin
          TextOut(X, Y, Parameters[i][j].Name.Text, clBlack);

          if (j < h) then
            TextOut(X, Y, ', ', clRed);

          if (j = h) then
          begin
            if (Parameters[i][j].VarType <> nil) then
            begin
              TextOut(X, Y, ': ', clRed);
              TextOut(X, Y, Parameters[i][j].VarType.Text, clBlack);
            end;

            if (i < High(Parameters)) then
              TextOut(X, Y, '; ', clRed);
          end;
        end;
      end;

      TextOut(X, Y, ')', clRed);
    end;

    if (FMethod.Header.Result <> nil) then
    begin
      TextOut(X, Y, ': ', clRed);
      TextOut(X, Y, FMethod.Header.Result.Text, clBlack);
    end;
  end;
end;

constructor TLapeTools_AutoComplete_Method.Create(AOwner: TTreeNodes; AutoComplete: TLapeTools_AutoComplete; Declaration: TDeclaration_Method);
begin
  inherited Create(AOwner, AutoComplete);

  FMethod := Declaration;
  FName := Declaration.Header.Name.Text;
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
  FName := Declaration.Name.Text;
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
begin
  X := DisplayLeft(True);
  Y := DisplayTop();

  TextOut(X, Y, FVariable.Name.Text, clBlack, [fsBold]);

  if (FVariable.VarType <> nil) then
  begin
    TextOut(X, Y, ': ', clRed);
    TextOut(X, Y, FVariable.VarType.Text, clBlack);
  end;

  if (FVariable.Value <> nil) then
  begin
    TextOut(X, Y, ' = ', clRed);
    TextOut(X, Y, SingleLine(FVariable.Value.Text), clBlack);
  end;
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
  FName := FLabel.Name.Text;
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
  FName := 'Self';
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
  FName := 'Result';
end;

