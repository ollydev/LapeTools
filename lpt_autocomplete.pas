unit lpt_autocomplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, SynEditKeyCmds, SynEdit, LMessages, LCLType, LCLIntf,
  lpt_editor, lpt_globals, lpt_parser;

type
  TLapeTools_AutoComplete = class;

  TLapeTools_AutoComplete_SizeDrag = class(TPanel)
  protected
    FDragging: Boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Paint; override;
  end;

  TLapeTools_AutoComplete_Popup_Header = class(TLabel)
  protected
    FAutoComplete: TLapeTools_AutoComplete;

    procedure TextOut(var X, Y: Int32; AText: String);
  public
    property AutoComplete: TLapeTools_AutoComplete read FAutoComplete write FAutoComplete;

    procedure Paint; override;
  end;

  TLapeTools_AutoComplete_Popup = class(TForm)
  protected
    FAutoComplete: TLapeTools_AutoComplete;
    FTimer: TTimer;
    FHeader: TLapeTools_AutoComplete_Popup_Header;
    FDescription: TLabel;
    FLocation: TLabel;
    FDeclaration: TDeclaration;

    procedure SetDeclaration(ADeclaration: TDeclaration);
    procedure SetDescription(ADescription: String);
    procedure SetHeader(AHeader: String);
    procedure SetAutoComplete(Value: TLapeTools_AutoComplete);

    procedure DoHide; override;
    procedure DoShow; override;
    procedure DoTimer(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure Paint; override;
  public
    property AutoComplete: TLapeTools_AutoComplete read FAutoComplete write SetAutoComplete;
    property Header: String write SetHeader;
    property Declaration: TDeclaration write SetDeclaration;
    property Description: String write SetDescription;

    constructor Create(AOwner: TComponent); override;
  end;

  TLapeTools_AutoComplete_Tree = class(TTreeView)
  protected
    FAutoComplete: TLapeTools_AutoComplete;

    procedure DoShowPopup(var Message: TLMessage); message CM_HINTSHOW;
    procedure DoChange(Sender: TObject; Node: TTreeNode);
    procedure DoChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure DoDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
  public
    property AutoComplete: TLapeTools_AutoComplete read FAutoComplete write FAutoComplete;

    procedure Add(Declaration: TDeclaration_Method); overload;
    procedure Add(Declaration: TDeclaration_Type_Record); overload;
    procedure Add(Declaration: TDeclaration_Variable); overload;
    procedure Add(Declaration: TDeclaration_Constant); overload;
    procedure Add(Declaration: TDeclaration_Type_Enum); overload;
    procedure Add(Declaration: TDeclaration_Label); overload;
    procedure Add(Declaration: TDeclaration_Type); overload;

    procedure AddField(Declaration: TDeclaration_Variable);
    procedure AddParameter(Declaration: TDeclaration_Variable);
    procedure AddSelf(Declaration: TDeclaration_Type);
    procedure AddResult(Declaration: TDeclaration_Type);

    constructor Create(AOwner: TComponent); override;
  end;

  TLapeTools_AutoComplete_Form = class(TForm)
  private
    FAutoComplete: TLapeTools_AutoComplete;
    FTree: TLapeTools_AutoComplete_Tree;
    FSizeDrag: TLapeTools_AutoComplete_SizeDrag;

    procedure DoHide; override;
  public
    property Tree: TLapeTools_AutoComplete_Tree read FTree;
    property AutoComplete: TLapeTools_AutoComplete read FAutoComplete write FAutoComplete;

    procedure Paint; override;

    constructor Create(AOwner: TComponent); override;
  end;

  TLapeTools_AutoComplete = class(TComponent)
  protected
    FParser: TLapeTools_Parser;
    FEditor: TLapeTools_Editor;
    FForm: TLapeTools_AutoComplete_Form;
    FTree: TLapeTools_AutoComplete_Tree;
    FPopup: TLapeTools_AutoComplete_Popup;
    FStartPos: TPoint;
    FEndPos: TPoint;
    FExpression: String;

    procedure DoApplicationLostFocus(Sender: TObject);
    procedure DoCommandProcessor(var Command: TSynEditorCommand; Char: TUTF8Char);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure Fill;
    procedure Filter;
    procedure Insert(Sender: TObject);

    procedure SetEditor(Value: TLapeTools_Editor);
  public
    property Tree: TLapeTools_AutoComplete_Tree read FTree;
    property Form: TLapeTools_AutoComplete_Form read FForm;
    property Popup: TLapeTools_AutoComplete_Popup read FPopup;
    property Editor: TLapeTools_Editor read FEditor write SetEditor;

    procedure Execute(X, Y: Int32);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math, SynEditHighlighter;

{$i lpt_autocomplete_types.inc}

procedure TLapeTools_AutoComplete_Popup_Header.TextOut(var X, Y: Int32; AText: String);
var
  Highlighter: TSynCustomHighlighter;
  TokStart: PChar;
  TokLen, i: Int32;
  TokString: String;
  Lines: TStringList;
begin
  Highlighter := FAutoComplete.Editor.Highlighter;

  Lines := TStringList.Create();
  Lines.Text := AText;

  for i := 0 to Lines.Count - 1 do
  begin
    Highlighter.ResetRange();
    Highlighter.SetLine(Lines[i], 0);

    while (not Highlighter.GetEol()) do
    begin
      Highlighter.GetTokenEx(TokStart, TokLen);

      if (TokLen > 0) then
      begin
        SetLength(TokString, TokLen);

        Move(TokStart^, TokString[1], TokLen);

        with Highlighter.GetTokenAttribute() do
        begin
          if (Foreground = clNone) then
            Canvas.Font.Color := clBlack
          else
            Canvas.Font.Color := ColorToRGB(Foreground);

          Canvas.Font.Style := Style;
          Canvas.TextOut(X, Y, TokString);

          X := X + Canvas.TextWidth(TokString);
        end;
      end;

      Highlighter.Next();
    end;

    X := 0;
    Y := Y + Canvas.TextHeight('TaylorSwift');
  end;

  Lines.Free();
end;

procedure TLapeTools_AutoComplete_Popup_Header.Paint;
var
  X, Y: Int32;
begin
  X := 0;
  Y := 0;

  TextOut(X, Y, Caption);
end;

procedure TLapeTools_AutoComplete_Popup.SetDeclaration(ADeclaration: TDeclaration);
begin
  FDeclaration := ADeclaration;

  FLocation.Visible := FDeclaration <> nil;

  if FLocation.Visible then
  begin
    FLocation.Font := FAutoComplete.Editor.Font;
    FLocation.Font.Color := clBlue;
    FLocation.Font.Underline := True;
    FLocation.BorderSpacing.Bottom := 6;

    FLocation.Caption := 'Line ' + IntToStr(FDeclaration.DocPos.Line) + ' in ' + FDeclaration.DocPos.FileName;
  end;
end;

procedure TLapeTools_AutoComplete_Popup.SetDescription(ADescription: String);
begin
  FDescription.Visible := ADescription <> '';

  if FDescription.Visible then
  begin
    FDescription.Font := FAutoComplete.Editor.Font;
    FDescription.Font.Color := clNavy;

    FDescription.Caption := ADescription;
    FDescription.BorderSpacing.Bottom := 5;

    FLocation.BorderSpacing.Bottom := 2;
  end;
end;

procedure TLapeTools_AutoComplete_Popup.SetHeader(AHeader: String);
begin
  FHeader.Visible := AHeader <> '';
  if FHeader.Visible then
  begin
    FHeader.Font := FAutoComplete.Editor.Font;
    FHeader.Font.Color := clBlack;
    FHeader.Font.Bold := True;

    FHeader.Caption := AHeader;
  end;
end;

procedure TLapeTools_AutoComplete_Popup.SetAutoComplete(Value: TLapeTools_AutoComplete);
begin
  FAutoComplete := Value;

  FHeader.AutoComplete := FAutoComplete;
end;

procedure TLapeTools_AutoComplete_Popup.DoHide;
begin
  if FAutoComplete.Form.CanFocus() then
    FAutoComplete.Form.SetFocus();

  FTimer.Enabled := False;
end;

procedure TLapeTools_AutoComplete_Popup.DoShow;
begin
  FTimer.Enabled := True;
end;

procedure TLapeTools_AutoComplete_Popup.DoTimer(Sender: TObject);
var
  P: TPoint;
begin
  P := Mouse.CursorPos;
  if (P.X < Left) or (P.Y < Top - 5) or (P.X > Left + Width) or (P.Y > Top + Height) then
    Hide();
end;

procedure TLapeTools_AutoComplete_Popup.DoClick(Sender: TObject);
begin
  if (FAutoComplete.Editor.OnShowDeclaration <> nil) then
  begin
    FAutoComplete.Editor.OnShowDeclaration(FDeclaration.DocPos.Line, FDeclaration.DocPos.Col, FDeclaration.DocPos.FileName);
    FAutoComplete.Form.Hide();
  end;
end;

procedure TLapeTools_AutoComplete_Popup.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Hide();

  if (Key = VK_ESCAPE) then
    FAutoComplete.Form.KeyDown(Key, Shift)
  else
    FAutoComplete.Tree.KeyDown(Key, Shift);
end;

procedure TLapeTools_AutoComplete_Popup.DoKeyPress(Sender: TObject; var Key: Char);
begin
  Hide();

  FAutoComplete.DoKeyPress(Sender, Key);
end;

procedure TLapeTools_AutoComplete_Popup.DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Hide();

  FAutoComplete.Tree.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TLapeTools_AutoComplete_Popup.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(ClientRect);
end;

constructor TLapeTools_AutoComplete_Popup.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  KeyPreview := True;
  ShowInTaskBar := stNever;
  Color := clWhite;

  OnKeyDown := @DoKeyDown;
  OnKeyPress := @DoKeyPress;
  OnMouseWheel := @DoMouseWheel;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @DoTimer;
  FTimer.Interval := 100;

  FHeader := TLapeTools_AutoComplete_Popup_Header.Create(Self);
  with FHeader do
  begin
    Parent := Self;
    AutoSize := True;
    Visible := False;
    Anchors := [akLeft, akTop];
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Self;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akTop].Control := Self;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
    BorderSpacing.Top := 2;
    OnMouseWheel := @Self.DoMouseWheel;
  end;

  FLocation := TLabel.Create(Self);
  with FLocation do
  begin
    Parent := Self;
    AutoSize := True;
    Visible := False;
    Cursor := crHandPoint;
    OnClick := @DoClick;
    Anchors := [akLeft, akTop];
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Self;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := FHeader;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
    OnMouseWheel := @Self.DoMouseWheel;
  end;

  FDescription := TLabel.Create(Self);
  with FDescription do
  begin
    Parent := Self;
    AutoSize := True;
    Visible := False;
    Anchors := [akLeft, akTop];
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Self;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := FLocation;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
    OnMouseWheel := @Self.DoMouseWheel;
  end;
end;

procedure TLapeTools_AutoComplete_SizeDrag.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FDragging := True;
end;

procedure TLapeTools_AutoComplete_SizeDrag.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FDragging then
    with Owner as TForm do
    begin
      Width := Max(Width + X, 300);
      Height := Max(Height + Y, 150);
    end;
end;

procedure TLapeTools_AutoComplete_SizeDrag.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FDragging := False;
end;

procedure TLapeTools_AutoComplete_SizeDrag.Paint;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clBtnShadow;
  Canvas.MoveTo(ClientRect.Right - 2, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right, ClientRect.Bottom - 3);
  Canvas.MoveTo(ClientRect.Right - 5, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right, ClientRect.Bottom - 6);
  Canvas.MoveTo(ClientRect.Right - 8, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right, ClientRect.Bottom - 9);
  Canvas.MoveTo(ClientRect.Right - 11, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right, ClientRect.Bottom - 12);
end;

procedure TLapeTools_AutoComplete_Tree.DoShowPopup(var Message: TLMessage);
var
  Node: TTreeNode;
  i: Int32;
begin
  Node := GetNodeAt(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y);

  if (Node <> nil) then
    with Node as TLapeTools_AutoComplete_Item, FAutoComplete.Popup do
    begin
      DrawPopup();

      for i := 0 to ControlCount - 1 do
        if Controls[i].Visible then
        begin
          Left := Mouse.CursorPos.X;
          Top := Mouse.CursorPos.Y + 5;
          Show();
        end;
    end;
end;

procedure TLapeTools_AutoComplete_Tree.DoChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node <> nil) then
    Node.Expand(True);
end;

procedure TLapeTools_AutoComplete_Tree.DoChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  if (not (csDestroying in ComponentState)) then
  begin
    FAutoComplete.Popup.Visible := False;

    if (Node <> nil) then
      Node.Collapse(True);
  end;
end;

procedure TLapeTools_AutoComplete_Tree.DoDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  if (Stage = cdPrePaint) then
  begin
    DefaultDraw := False;

    with Node as TLapeTools_AutoComplete_Item do
    begin
      DrawBackground();
      DrawColumn();
      DrawText();
    end;
  end;
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Method);
begin
  Items.AddNode(TLapeTools_AutoComplete_Method.Create(Items, FAutoComplete, Declaration), nil, Declaration.Header.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Type_Record);
begin
  Items.AddNode(TLapeTools_AutoComplete_Record.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Variable);
begin
  Items.AddNode(TLapeTools_AutoComplete_Variable.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Constant);
begin
  Items.AddNode(TLapeTools_AutoComplete_Constant.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Type_Enum);
var
  i: Int32;
  Element: TDeclaration_EnumElement;
begin
  Items.AddNode(TLapeTools_AutoComplete_Type.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);

  for i := 0 to Declaration.Elements.Count - 1 do
  begin
    Element := Declaration.Elements.Get(i) as TDeclaration_EnumElement;

    Items.AddNode(TLapeTools_AutoComplete_Enum.Create(Items, FAutoComplete, Element), nil, Element.Name.Text, nil, naAdd);
  end;
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Label);
begin
  Items.AddNode(TLapeTools_AutoComplete_Label.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.Add(Declaration: TDeclaration_Type);
begin
  Items.AddNode(TLapeTools_AutoComplete_Type.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.AddField(Declaration: TDeclaration_Variable);
begin
  Items.AddNode(TLapeTools_AutoComplete_Field.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.AddParameter(Declaration: TDeclaration_Variable);
begin
  Items.AddNode(TLapeTools_AutoComplete_Parameter.Create(Items, FAutoComplete, Declaration), nil, Declaration.Name.Text, nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.AddSelf(Declaration: TDeclaration_Type);
begin
  Items.AddNode(TLapeTools_AutoComplete_Self.Create(Items, FAutoComplete, Declaration), nil, 'Self', nil, naAdd);
end;

procedure TLapeTools_AutoComplete_Tree.AddResult(Declaration: TDeclaration_Type);
begin
  Items.AddNode(TLapeTools_AutoComplete_Result.Create(Items, FAutoComplete, Declaration), nil, 'Result', nil, naAdd);
end;

constructor TLapeTools_AutoComplete_Tree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ScrollBars := ssBoth;
  ShowRoot := False;
  BorderStyle := bsNone;
  DoubleBuffered := True;
  ToolTips := False;
  Options := Options - [tvoThemedDraw, tvoNoDoubleClickExpand, tvoShowRoot, tvoShowLines, tvoShowSeparators, tvoShowButtons] + [tvoReadOnly];

  OnAdvancedCustomDrawItem := @DoDrawItem;
  OnChanging := @DoChanging;
  OnChange := @DoChange;
end;

procedure TLapeTools_AutoComplete_Form.DoHide;
begin
  FAutoComplete.Popup.Hide();

  if FAutoComplete.Editor.CanFocus() then
    FAutoComplete.Editor.SetFocus();
end;

procedure TLapeTools_AutoComplete_Form.Paint;
begin
  Canvas.Brush.Color := clForm;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(ClientRect);
end;

constructor TLapeTools_AutoComplete_Form.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  BorderStyle := bsNone;
  KeyPreview := True;
  ShowInTaskBar := stNever;
  Width := 450;
  Height := 200;

  FTree := TLapeTools_AutoComplete_Tree.Create(Self);
  with FTree do
  begin
    Parent := Self;
    Align := alClient;
    BorderSpacing.Around := 1;
  end;

  FSizeDrag := TLapeTools_AutoComplete_SizeDrag.Create(Self);
  with FSizeDrag do
  begin
    Parent := Self;
    Width := GetSystemMetrics(SM_CXVSCROLL) - 1;
    Height := GetSystemMetrics(SM_CXVSCROLL) - 1;
    Cursor := crSizeNWSE;
    Anchors := [akBottom, akRight];
    AnchorSideRight.Side := asrBottom;
    AnchorSideRight.Control := Self;
    AnchorSideBottom.Side := asrBottom;
    AnchorSideBottom.Control := Self;
    BorderSpacing.Right := 1;
    BorderSpacing.Bottom := 1;
  end;
end;

procedure TLapeTools_AutoComplete.DoKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #32..#122:
      begin
        FEditor.CommandProcessor(ecChar, Key, nil);

        if (not (Key in FEditor.IdentChars)) then
          FForm.Hide();
      end;
    #8:
      FEditor.CommandProcessor(ecDeleteLastChar, #0, nil);
  end;

  Filter();
end;

procedure TLapeTools_AutoComplete.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        Insert(nil);

        Key := VK_UNKNOWN;
      end;

    VK_ESCAPE:
      begin
        FForm.Hide();

        Key := VK_UNKNOWN;
      end;
  end;
end;

procedure TLapeTools_AutoComplete.Fill;

  procedure addMap(Map: TDeclarationMap);
  var
    i: Int32;
    Declaration: TDeclaration;
  begin
    for i := 0 to Map.Count - 1 do
    begin
      Declaration := Map.Get(i);
      if (not Declaration.InScope) then
        Continue;
      if (Declaration is TDeclaration_Method) and (TDeclaration_Method(Declaration).Header.MethodType in [mtFunctionOfObject, mtProcedureOfObject]) then
        Continue;

      if Declaration is TDeclaration_Method then
        FTree.Add(Declaration as TDeclaration_Method)
      else if Declaration is TDeclaration_Type_Record then
        FTree.Add(Declaration as TDeclaration_Type_Record)
      else if (Declaration is TDeclaration_Constant) then
        FTree.Add(Declaration as TDeclaration_Constant)
      else if (Declaration is TDeclaration_Variable) then
        FTree.Add(Declaration as TDeclaration_Variable)
      else if (Declaration is TDeclaration_Type_Enum) then
        FTree.Add(Declaration as TDeclaration_Type_Enum)
      else if (Declaration is TDeclaration_Label) then
        FTree.Add(Declaration as TDeclaration_Label)
      else if (Declaration is TDeclaration_Type) then
        FTree.Add(Declaration as TDeclaration_Type);
    end;
  end;

  procedure addMembers(Declaration: TDeclaration; isParent: Boolean = False);
  var
    i: Int32;
    Method: TDeclaration_Method;
    Parent: TDeclaration_Type;
  begin
    if (Declaration <> nil) and (Declaration is TDeclaration_Type) then
      with Declaration as TDeclaration_Type do
      begin
        if Declaration is TDeclaration_Type_Record then
          with Declaration as TDeclaration_Type_Record do
            for i := 0 to Fields.Count - 1 do
               FTree.AddField(Fields.Get(i) as TDeclaration_Variable);

        if (Name <> nil) then
          for Method in Self.FParser.Map.GetMethods(Name.Text) do
            FTree.Add(Method);

        if (not isParent) then
          for Parent in Parents(Self.FParser) do
            addMembers(Parent, True);
      end;
  end;

  procedure addLocals(Method: TDeclaration_Method);
  var
    i: Int32;
    Declaration: TDeclaration_Type;
  begin
    addMap(Method.Locals);

    with Method.Header do
    begin
      for i := 0 to Parameters.Count - 1 do
        FTree.AddParameter(Parameters.Get(i));

      if (MethodType in [mtProcedureOfObject, mtFunctionOfObject]) and (FParser.Map.GetType(ObjectName.Text) <> nil) then
      begin
        Declaration := FParser.Map.GetType(ObjectName.Text);

        if (Declaration <> nil) then
        begin
          FTree.AddSelf(Declaration);

          addMembers(Declaration);
        end;
      end;

      if (MethodType in [mtFunctionOfObject, mtFunction]) then
        FTree.AddResult(Result);
    end;

    if (Method.Parent <> nil) and (not (mdStatic in Method.Header.Directives)) then
      addLocals(Method.Parent);
  end;

begin
  FTree.BeginUpdate();
  FTree.Items.Clear();

  if (Pos('.', FExpression) > 0) then
    addMembers(FParser.ParseExpression(Copy(FExpression, 1, LastDelimiter('.', FExpression)), True))
  else
  begin
    if (FParser.InMethod <> nil) then
      addLocals(FParser.InMethod);

    addMap(FParser.Map);
  end;

  FTree.AlphaSort();
  FTree.EndUpdate();
end;

procedure TLapeTools_AutoComplete.Filter;
var
  i: Int32;
  Word: String;
  Node: TTreeNode;
begin
  Word := LowerCase(FEditor.GetWordAtRowCol(FEditor.CaretXY));

  try
    FTree.BeginUpdate();

    for i := 0 to FTree.Items.TopLvlCount - 1 do
    begin
      Node := FTree.Items.TopLvlItems[i];
      Node.Visible := (Word = '') or (Pos(Word, LowerCase(Node.Text)) = 1);
    end;

    Node := FTree.Items.GetFirstVisibleNode();
    if (Node <> nil) then
      Node.Selected := True;
  finally
    FTree.EndUpdate();
  end;
end;

procedure TLapeTools_AutoComplete.DoApplicationLostFocus(Sender: TObject);
begin
  FForm.Hide();
end;

procedure TLapeTools_AutoComplete.DoCommandProcessor(var Command: TSynEditorCommand; Char: TUTF8Char);
begin
  case Command of
    lecAutoComplete:
      begin
        if (FParser <> nil) then
          FParser.Free();

        FParser := FEditor.GetParser(True);
        FStartPos := FEditor.CaretXY;
        FEndPos := FEditor.CaretXY;
        FExpression := FEditor.GetExpression(FStartPos, FEndPos);

        Fill();
        Filter();

        with FEditor.ClientToScreen(FEditor.RowColumnToPixels(FStartPos)) do
          Execute(X, Y + FEditor.LineHeight);
      end;

    lecFocus:
      begin
        if FPopup.Visible then
          FPopup.Visible := False;
        if FForm.Visible then
          FForm.Visible := False;
      end;

    lecCaretChange:
      begin
        if (FEditor.CaretX <= FEndPos.X) and (FEditor.CaretY <= FEndPos.Y) and (FEditor.CaretChar in [#0, #32, '.']) then
        begin
          if FPopup.Visible then
            FPopup.Visible := False;
          if FForm.Visible then
            FForm.Visible := False;
        end;
      end;
  end;
end;

procedure TLapeTools_AutoComplete.Insert(Sender: TObject);
begin
  if (FTree.Selected <> nil) then
    FEditor.TextBetweenPointsEx[FStartPos, Point(Max(FEndPos.X, FEditor.CaretX), FEndPos.Y), scamEnd] := FTree.Selected.Text;

  FForm.Hide();
end;

procedure TLapeTools_AutoComplete.SetEditor(Value: TLapeTools_Editor);
begin
  FEditor := Value;
  FEditor.AddCommandProcessor(@DoCommandProcessor);
end;

procedure TLapeTools_AutoComplete.Execute(X, Y: Int32);
begin
  if (not FTree.Font.IsEqual(FEditor.Font))  then
  begin
    FTree.Font := FEditor.Font;
    FTree.DefaultItemHeight := FTree.Canvas.TextHeight('TaylorSwift');
  end;

  FForm.Left := X;
  FForm.Top := Y;
  FForm.Show();
end;

constructor TLapeTools_AutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := TLapeTools_AutoComplete_Form.Create(Self);
  with FForm do
  begin
    OnKeyPress := @Self.DoKeyPress;
    OnKeyDown := @Self.DoKeyDown;
    KeyPreview := True;
    AutoComplete := Self;
  end;

  FTree := FForm.Tree;
  with FTree do
  begin
    ShowHint := True;
    OnDblClick := @Insert;
    AutoComplete := Self;
  end;

  FPopup := TLapeTools_AutoComplete_Popup.Create(Self);
  with FPopup do
  begin
    AutoSize := True;
    BorderStyle := bsNone;
    AutoComplete := Self;
  end;

  Application.AddOnDeactivateHandler(@DoApplicationLostFocus);
end;

destructor TLapeTools_AutoComplete.Destroy;
begin
  if (FParser <> nil) then
    FParser.Free();

  Application.RemoveOnDeactivateHandler(@DoApplicationLostFocus);

  inherited Destroy();
end;

end.

