unit lpt_codeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, TreeFilterEdit,
  lptypes, lpt_editor, lpt_parser;

type
  TLapeTools_CodeView = class;

  TLapeTools_CodeView_Updater = class(TThread)
  protected
    FCodeView: TLapeTools_CodeView;
    FParser: TLapeTools_Parser;
    FChangeStamp: Int64;

    procedure Initalize;
    procedure Finalize;

    procedure Execute; override;
  public
    property ChangeStamp: Int64 read FChangeStamp write FChangeStamp;

    constructor Create(CodeView: TLapeTools_CodeView);
  end;

  TLapeTools_CodeView_Node = class(TTreeNode)
  public
    DocPos: TDocPos;

    constructor Create(AOwner: TTreeNodes; Declaration: TDeclaration); reintroduce;
  end;

  TLapeTools_CodeView = class(TPanel)
  protected
    FTree: TTreeView;
    FUpdater: TLapeTools_CodeView_Updater;
    FEditor: TLapeTools_Editor;
    FFilter: TTreeFilterEdit;

    procedure DoClick(Sender: TObject);

    procedure SetEditor(AEditor: TLapeTools_Editor);
  public
    property Editor: TLapeTools_Editor read FEditor write SetEditor;
    property Tree: TTreeView read FTree write FTree;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lpt_globals;

constructor TLapeTools_CodeView_Node.Create(AOwner: TTreeNodes; Declaration: TDeclaration);
begin
  inherited Create(AOwner);

  DocPos := Declaration.DocPos;
end;

procedure TLapeTools_CodeView.DoClick(Sender: TObject);
begin
  if (FTree.Selected <> nil) and (FEditor.OnShowDeclaration <> nil) then
    with FTree.Selected as TLapeTools_CodeView_Node do
      FEditor.OnShowDeclaration(DocPos.Line, DocPos.Col, DocPos.FileName);
end;

procedure TLapeTools_CodeView.SetEditor(AEditor: TLapeTools_Editor);
begin
  FEditor := AEditor;

  FUpdater.ChangeStamp := 0;
end;

constructor TLapeTools_CodeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := '';
  BevelOuter := bvNone;

  FTree := TTreeView.Create(Self);
  with FTree do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    Align := alClient;
    ScrollBars:= ssAutoBoth;
    Images := LapeTools_Images;
    ReadOnly := True;
    OnDblClick := @DoClick;
  end;

  FFilter := TTreeFilterEdit.Create(Self);
  with FFilter do
  begin
    Parent := Self;
    Align := alBottom;
    FilteredTreeView := FTree;
    ButtonWidth := 0;
  end;

  FUpdater := TLapeTools_CodeView_Updater.Create(Self);
  FUpdater.FreeOnTerminate := True;
end;

destructor TLapeTools_CodeView.Destroy;
begin
  FUpdater.Terminate();

  inherited Destroy();
end;

procedure TLapeTools_CodeView_Updater.Execute;
var
  i: Int32;
  Declaration: TDeclaration;
begin
  while (not Terminated) do
  begin
    Synchronize(@Initalize);

    if (FParser <> nil) then
    begin
      FParser.Parse();

      for i := 0 to FParser.Map.Count - 1 do
      begin
        Declaration := FParser.Map.Get(i);
        Declaration.DocPos;

        if (Declaration is TDeclaration_Variable) then
          with Declaration as TDeclaration_Variable do
            Declaration.Text := Name.Text
        else
        if (Declaration is TDeclaration_Type) then
          with Declaration as TDeclaration_Type do
            Declaration.Text := Name.Text
        else
        if (Declaration is TDeclaration_Label) then
          with Declaration as TDeclaration_Label do
            Declaration.Text := Name.Text
        else
        if (Declaration is TDeclaration_Method) then
          with Declaration as TDeclaration_Method do
          begin
            if (Header.MethodType in [mtFunctionOfObject, mtProcedureOfObject]) then
              Declaration.Text := Header.ObjectName.Text + '.' + Header.Name.Text
            else
              Declaration.Text := Header.Name.Text;
          end;
      end;

      Synchronize(@Finalize);
    end;

    Sleep(650);
  end;
end;

procedure TLapeTools_CodeView_Updater.Initalize;
begin
  if (FParser <> nil) then
    FParser.Free();
  FParser := nil;

  if (FCodeView.Editor <> nil) and (FCodeView.Editor.ChangeStamp <> FChangeStamp) then
  begin
    FParser := FCodeView.Editor.GetParser(False, False);
    FParser.Settings := FParser.Settings - [psParseIncludes, psAddMethodUnderType];

    FChangeStamp := FCodeView.Editor.ChangeStamp;
  end;
end;

procedure TLapeTools_CodeView_Updater.Finalize;
var
  Node: TTreeNode;
  Declaration: TDeclaration;
  i: Int32;
begin
  with FCodeView.Tree do
  begin
    Items.BeginUpdate();
    Items.Clear();

    for i := 0 to FParser.Map.Count - 1 do
    begin
      Declaration := FParser.Map.Get(i);

      Node := Items.AddNode(TLapeTools_CodeView_Node.Create(Items, Declaration), nil, Declaration.Text, nil, naAdd);
      Node.ImageIndex := GetImage(Declaration);
      Node.SelectedIndex := GetImage(Declaration);
    end;

    Items.EndUpdate();
  end;
end;

constructor TLapeTools_CodeView_Updater.Create(CodeView: TLapeTools_CodeView);
begin
  inherited Create(False);

  FCodeView := CodeView;
  FParser := nil;
  FChangeStamp := 0;
end;

end.

