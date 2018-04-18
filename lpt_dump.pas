unit lpt_Dump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lpvartypes, lpparser, lptypes, lptree;

type
  TLapeTools_Dump = class(TLapeCompiler)
  protected
    FDump: TStringList;
    FDumpDirectory: String;

    procedure add(Header: lpString; Method: Boolean = False; Define: Boolean = False);
  public
    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; overload; override;
    function addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar; overload; override;
    function addGlobalMethod(AHeader: lpString; Value: TMethod): TLapeGlobalVar; overload; override;

    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; overload; override;

    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: ShortString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiChar; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: WideChar; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;

    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean=True): TLapeTree_Base; override;

    procedure addBaseDefine(Define: lpString); override;

    constructor Create(Directory: String); overload;
    destructor Destroy; override;
  end;

implementation

procedure TLapeTools_Dump.add(Header: lpString; Method: Boolean; Define: Boolean);
begin
  Header := Trim(Header);

  if (Header <> '') and (FBaseDefines.Values['IMPORT_SECTION'] <> '') then
  begin
    if (not Define) and (Header[Length(Header)] <> ';') then
      Header := Header + ';';
    if Method then
      Header := Header + ' begin end;';
    Header := Header + LineEnding;

    FDump.Values[FBaseDefines.Values['IMPORT_SECTION']] := FDump.Values[FBaseDefines.Values['IMPORT_SECTION']] + Header;
  end;
end;

procedure TLapeTools_Dump.addBaseDefine(Define: lpString);
begin
  add('{$DEFINE ' + Define + '}', False, True);

  inherited addBaseDefine(Define);
end;

function TLapeTools_Dump.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
begin
  add(AHeader, True);

  Result := inherited addGlobalFunc(AHeader, Value);
end;

function TLapeTools_Dump.addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar;
begin
  add(AHeader, True);

  Result := inherited addGlobalMethod(AHeader, AMethod, ASelf);
end;

function TLapeTools_Dump.addGlobalMethod(AHeader: lpString; Value: TMethod): TLapeGlobalVar;
begin
  add(AHeader, True);

  Result := inherited addGlobalMethod(AHeader, Value);
end;

function TLapeTools_Dump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  add(Format('type %s = %s', [AName, Str]));

  Result := inherited addGlobalType(Str, AName);
end;

function TLapeTools_Dump.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  if (AName <> '') and (AName[1] <> '!') then
    add(Format('type %s = %s', [AName, AName]));

  Result := inherited addGlobalType(Typ, AName, ACopy);
end;

function TLapeTools_Dump.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: %s', [AName, Typ]));

  Result := inherited addGlobalVar(Typ, Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: Int32 = %d', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: UInt32 = %d', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: Int64 = %d', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: UInt64 = %d', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: Extended = %f', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: ShortString; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: ShortString = %s', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: String = %s', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: UnicodeString = %s', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: AnsiChar; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: AnsiChar = %s', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: WideChar; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: WideChar = %s', [AName, Value]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: Variant', [AName]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  add(Format('var %s: Pointer = %s', [AName, PointerToString(Value)]));

  Result := inherited addGlobalVar(Value, AName);
end;

function TLapeTools_Dump.addDelayedCode(ACode: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  if (AFileName <> '') and (AFileName[1] = '!') then
    { nothing }
  else
    add(ACode);

  Result := inherited addDelayedCode(ACode, AFileName, AfterCompilation, IsGlobal);
end;

constructor TLapeTools_Dump.Create(Directory: String);
begin
  inherited Create(TLapeTokenizerString.Create('begin end.'));

  FDumpDirectory := Directory;

  FDump := TStringList.Create();
  FDump.Values['System'] := {$i lpt_compiler_imports_system.inc}
  FDump.Values['Math'] := {$i lpt_compiler_imports_math.inc}
  FDump.Values['Variant'] := {$i lpt_compiler_imports_variant.inc}
  FDump.Values['DateTime'] := {$i lpt_compiler_imports_datetime.inc}
  FDump.Values['String'] := {$i lpt_compiler_imports_string.inc}
  FDump.Values['External'] := {$i lpt_compiler_imports_external.inc}
end;

destructor TLapeTools_Dump.Destroy;
var
  i: Int32;
begin
  ForceDirectories(FDumpDirectory);

  for i := 0 to FDump.Count - 1 do
    with TStringList.Create() do
    try
      Text := FDump.Values[FDump.Names[i]];

      SaveToFile(IncludeTrailingPathDelimiter(FDumpDirectory) + FDump.Names[i] + '.dump');
    finally
      Free();
    end;

  FDump.Free();

  inherited Destroy();
end;

end.

