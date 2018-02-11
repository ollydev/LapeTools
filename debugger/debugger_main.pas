unit debugger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, PairSplitter, Menus, SynHighlighterPas,
  lptypes, lpt_editor, lpt_autocomplete, lpt_codeview, lpt_parameterhint;

type
  TDebugForm = class(TForm)
    lblParseTime: TLabel;
    Highlighter: TSynFreePascalSyn;
    PairSplitter: TPairSplitter;
    pnlView: TPairSplitterSide;
    pnlEditor: TPairSplitterSide;
    ToolBar: TToolBar;
    btnParse: TToolButton;

    procedure DoCreate(Sender: TObject);
    procedure DoParse(Sender: TObject);
  public
    Editor: TLapeTools_Editor;
    CodeView: TLapeTools_CodeView;

    function LibraryDirective(Argument: String): String;
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
  ParameterHint: TLapeTools_ParameterHint;
begin
  Editor := TLapeTools_Editor.Create(Self);
  with Editor do
  begin
    Parent := pnlEditor;
    Align := alClient;
    Highlighter := Self.Highlighter;
    OnShowDeclaration := @ShowDeclaration;
    OnLibraryDirective := @LibraryDirective;
    Paths.Add('Includes/');
    InternalIncludes.Add('imports/imports.simba');
    Load('default.simba');
  end;

  CodeView := TLapeTools_CodeView.Create(Self);
  with CodeView do
  begin
    Parent := pnlView;
    Align := alClient;
    Editor := Self.Editor;
  end;

  AutoComplete := TLapeTools_AutoComplete.Create(Self);
  AutoComplete.Editor := Editor;

  ParameterHint := TLapeTools_ParameterHint.Create(Self);
  ParameterHint.Editor := Editor;
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

function TDebugForm.LibraryDirective(Argument: String): String;
begin
  if (Argument <> 'SimpleOCR') then
    Exit('');

  Result := 'type TFontChar = packed record' + LineEnding +
            '  FChar:AnsiChar;' + LineEnding +
            '  FWidth,FHeight:Int32;' + LineEnding +
            '  loaded, hasShadow:LongBool;' + LineEnding +
            '  pts,shadow,bad:TPointArray;' + LineEnding +
            'end;' + LineEnding +
            'type TFontset = packed record' + LineEnding +
            '  FData: Array of TFontChar;' + LineEnding +
            '  SpaceWidth: Int32;' + LineEnding +
            'end;' + LineEnding +
            'type TCompareRules = packed record' + LineEnding +
            '  Color, ColorMaxDiff: Int32;' + LineEnding +
            '  UseShadow: LongBool;' + LineEnding +
            '  ShadowMaxValue:Int32;' + LineEnding +
            '  Threshold: Int32;' + LineEnding +
            '  ThreshInv: LongBool;' + LineEnding +
            'end;' + LineEnding +
            'type TSimpleOCR = packed record' + LineEnding +
            '  IsLoaded: LongBool;' + LineEnding +
            '  FontData: TFontSet;' + LineEnding +
            '  ClientID: TTarget_Exported;' + LineEnding +
            '  Client:   T2DIntArray;' + LineEnding +
            '  __maxShadowAvg: Int32;' + LineEnding +
            '  __debugging: LongBool;' + LineEnding +
            'end;' + LineEnding +
            'procedure TFontSet.Load(Font:AnsiString; Space:Int32=4); begin end;' + LineEnding +
            'procedure TFontSet.Free(); begin end;' + LineEnding +
            'procedure TSimpleOCR.Init(FontPath:TFontSet; AClient:TTarget_Exported=ExportImageTarget()); begin end;' + LineEnding +
            'procedure TSimpleOCR.Init(Font:AnsiString; SpaceWidth:Int32=4; AClient:TTarget_Exported=ExportImageTarget()); overload; begin end;' + LineEnding +
            'procedure TSimpleOCR.SetFont(FontPath:TFontSet); begin end;' + LineEnding +
            'procedure TSimpleOCR.SetFont(Font:AnsiString; SpaceWidth:Int32=4); overload; begin end;' + LineEnding +
            'procedure TSimpleOCR.Free(); begin end;' + LineEnding +
            'function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; begin end;' + LineEnding +
            'function TSimpleOCR.RecognizeEx(AClient:T2DIntArray; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; begin end;';
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

