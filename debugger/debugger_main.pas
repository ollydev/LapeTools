unit debugger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, PairSplitter, Menus, SynHighlighterPas,
  lptypes, lpt_editor, lpt_AutoComplete;

type
  TDebugForm = class(TForm)
    lblParseTime: TLabel;
    Highlighter: TSynFreePascalSyn;
    PairSplitter: TPairSplitter;
    pnlTrees: TPairSplitterSide;
    pnlEditor: TPairSplitterSide;
    ToolBar: TToolBar;
    btnParse: TToolButton;

    procedure DoCreate(Sender: TObject);
    procedure DoParse(Sender: TObject);
  public
    Editor: TLapeTools_Editor;

    procedure ShowDeclaration(Line, Column: Int32; FilePath: String);
  end;

var
  DebugForm: TDebugForm;

implementation

{$IFDEF WINDOWS}
uses
  Windows;
{$ENDIF}

function MarkTime(): Double;
var
  {$IFDEF WINDOWS}
  Frequency, Count: Int64;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Count);
  Result := Count / Frequency * 1000;
  {$ELSE}
  Result := Now();
  {$ENDIF}
end;

procedure TDebugForm.DoCreate(Sender: TObject);
var
  AutoComplete: TLapeTools_AutoComplete;
begin
  Editor := TLapeTools_Editor.Create(Self);
  with Editor do
  begin
    Parent := pnlEditor;
    Align := alClient;
    Highlighter := Self.Highlighter;
    OnShowDeclaration := @ShowDeclaration;
    Paths.Add('Includes/');
    Load('default.simba');
  end;

  AutoComplete := TLapeTools_AutoComplete.Create(Self);
  AutoComplete.Editor := Editor;
end;

procedure TDebugForm.DoParse(Sender: TObject);
var
  T: Double;
begin
  T := MarkTime();

  with Editor.GetParser(True) do
  try
    lblParseTime.Caption := Format('%f (%d Items)', [MarkTime() - T, Map.Count]);
  finally
    Free();
  end;
end;

procedure TDebugForm.ShowDeclaration(Line, Column: Int32; FilePath: String);
begin
  if (FilePath = Editor.FilePath) then
    Editor.SelectWord(Column, Line)
  else
    WriteLn('Declared in "', FilePath, '" at line: ', Line);
end;

{$R *.lfm}

end.

