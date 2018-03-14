unit lpt_scriptparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,
  lptypes, lpparser,
  lpt_parser;

type
  TLapeTools_ScriptParser = class;

  TLapeTools_CachedFile = class
  protected
    FRefCount: Int32;
    FParser: TLapeTools_Parser;
    FDelete: Boolean;
  public
    property Delete: Boolean read FDelete;

    procedure IncRef;
    procedure DecRef;

    function Equals(Sender: TLapeTools_Parser; FilePath: String): Boolean; virtual; abstract;
    procedure AddToParser(Parser: TLapeTools_ScriptParser); virtual; abstract;

    constructor Create(Sender: TLapeTools_Parser; FilePath: String); virtual; abstract;
    destructor Destroy; override;
  end;

  TLapeTools_CachedInclude = class(TLapeTools_CachedFile)
  protected
    FDefines: record Input, Output: TStringList; end;
  public
    function Equals(Sender: TLapeTools_Parser; FilePath: String): Boolean; override;
    procedure AddToParser(Destination: TLapeTools_ScriptParser); override;

    constructor Create(Sender: TLapeTools_Parser; FilePath: String); override;
    destructor Destroy; override;
  end;

  TLapeTools_CachedMainFile = class(TLapeTools_CachedFile)
  protected
    FSenderPath: String;
  public
    function Equals(Sender: TLapeTools_Parser; FilePath: String): Boolean; override;
    procedure AddToParser(Destination: TLapeTools_ScriptParser); override;

    constructor Create(Sender: TLapeTools_Parser; FilePath: String); override;
  end;

  TLapeTools_CachedClass = class of TLapeTools_CachedFile;

  TLapeTools_Cache = class(specialize TLapeList<TLapeTools_CachedFile>)
  protected
    FCriticalSection: TCriticalSection;
  public
    function Get(AClass: TLapeTools_CachedClass; Sender: TLapeTools_Parser; FilePath: String): TLapeTools_CachedFile;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TLapeTools_ScriptParser = class(TLapeTools_Parser)
  public
    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; override; // public to add internal includes easily
  public
    CachedFiles: array of TLapeTools_CachedFile;

    destructor Destroy; override;
  end;

implementation

var
  Cache: TLapeTools_Cache;

function TLapeTools_CachedMainFile.Equals(Sender: TLapeTools_Parser; FilePath: String): Boolean;
begin
  Result := SameFileName(FilePath, FParser.FilePath) and SameFileName(FSenderPath, Sender.FilePath);
end;

procedure TLapeTools_CachedMainFile.AddToParser(Destination: TLapeTools_ScriptParser);
var
  i: Int32;
begin
  with FParser.Map.ExportToArrays() do
    for i := 0 to High(Keys) do
      if Items[i].InScope then
        Destination.Map.Add(Keys[i], Items[i]);

  SetLength(Destination.CachedFiles, Length(Destination.CachedFiles) + 1);
  Destination.CachedFiles[High(Destination.CachedFiles)] := Self;
end;

constructor TLapeTools_CachedMainFile.Create(Sender: TLapeTools_Parser; FilePath: String);
var
  i: Int32;
begin
  FSenderPath := ExpandFileName(Sender.FilePath);

  FParser := TLapeTools_Parser.Create(FilePath);
  FParser.OnLibraryDirective := Sender.OnLibraryDirective;
  FParser.Paths.AddStrings(Sender.Paths);
  FParser.Parse();

  // make everything visible utill we reach the current file
  for i := 0 to FParser.Map.Count - 1 do
  begin
    if SameFileName(ExpandFileName(FParser.Map.Get(i).DocPos.FileName), FSenderPath) then
      Break;

    FParser.Map.Get(i).InScope := True;
  end;

  // make everything hidden in the current file
  for i := i to FParser.Map.Count - 1 do
  begin
    if (not SameFileName(ExpandFileName(FParser.Map.Get(i).DocPos.FileName), FSenderPath)) then
      Break;

    FParser.Map.Get(i).InScope := False;
  end;

  // make everything hidden apart from method of objects (for lape's type forwarding)
  for i := i to FParser.Map.Count - 1 do
    FParser.Map.Get(i).InScope := (FParser.Map.Get(i) is TDeclaration_Method) and (TDeclaration_Method(FParser.Map.Get(i)).Header.MethodType in [mtFunctionOfObject, mtProcedureOfObject]);

  for i := 0 to FParser.Includes.Count - 1 do
    FParser.Includes.Objects[i] := TObject(FileAge(FParser.Includes[i]));
end;

function TLapeTools_CachedInclude.Equals(Sender: TLapeTools_Parser; FilePath: String): Boolean;
var
  i: Int32;
begin
  if (FRefCount = 0) then
  begin
    for i := 0 to FParser.Includes.Count - 1 do
      if FileAge(FParser.Includes[i]) <> Int32(FParser.Includes.Objects[i]) then
        FDelete := True;
  end;

  Result := (not FDelete) and SameFileName(FParser.FilePath, FilePath) and FDefines.Input.Equals(Sender.Defines);
end;

procedure TLapeTools_CachedInclude.AddToParser(Destination: TLapeTools_ScriptParser);
var
  i: Int32;
begin
  Destination.Includes.Add(FParser.FilePath);
  Destination.Defines.AddStrings(FDefines.Output);

  with FParser.Map.ExportToArrays() do
    for i := 0 to High(Keys) do
      Destination.Map.Add(Keys[i], Items[i]);

  SetLength(Destination.CachedFiles, Length(Destination.CachedFiles) + 1);
  Destination.CachedFiles[High(Destination.CachedFiles)] := Self;
end;

constructor TLapeTools_CachedInclude.Create(Sender: TLapeTools_Parser; FilePath: String);
var
  i: Int32;
begin
  FDefines.Input := TStringList.Create();
  FDefines.Input.AddStrings(Sender.Defines);
  FDefines.Output := TStringList.Create();

  FParser := TLapeTools_Parser.Create(FilePath);
  FParser.OnLibraryDirective := Sender.OnLibraryDirective;
  FParser.Defines.AddStrings(Sender.Defines);
  FParser.Paths.AddStrings(Sender.Paths);
  FParser.Parse();

  for i := 0 to FParser.Defines.Count - 1 do
    if (FDefines.Input.IndexOf(FParser.Defines[i]) = -1) then
      FDefines.Output.Add(FParser.Defines[i]);

  FParser.Includes.Add(FilePath);
  for i := 0 to FParser.Includes.Count - 1 do
    FParser.Includes.Objects[i] := TObject(FileAge(FParser.Includes[i]));
end;

destructor TLapeTools_CachedInclude.Destroy;
begin
  FDefines.Input.Free();
  FDefines.Output.Free();

  inherited Destroy();
end;

procedure TLapeTools_CachedFile.IncRef;
begin
  FRefCount += 1;
end;

procedure TLapeTools_CachedFile.DecRef;
begin
  FRefCount -= 1;
end;

destructor TLapeTools_CachedFile.Destroy;
begin
  if (FParser <> nil) then
    FParser.Free();
end;

function TLapeTools_Cache.Get(AClass: TLapeTools_CachedClass;Sender: TLapeTools_Parser; FilePath: String): TLapeTools_CachedFile;
var
  i: Int32;
begin
  Result := nil;

  FCriticalSection.Enter();

  try
    for i := Count - 1 downto 0 do
      if Items[i].Delete then
        Delete(i).Free();

    for i := 0 to Count - 1 do
      if (Items[i].ClassType = AClass) and Items[i].Equals(Sender, FilePath) then
        Result := Items[i];

    if (Result = nil) then
      Result := Items[Add(AClass.Create(Sender, FilePath))];

    Result.IncRef();
  finally
    FCriticalSection.Leave();
  end;
end;

constructor TLapeTools_Cache.Create;
begin
  inherited Create(nil, dupAccept, False);

  FCriticalSection := TCriticalSection.Create();
end;

destructor TLapeTools_Cache.Destroy;
begin
  FCriticalSection.Free();

  while (Count > 0) do
    Delete(0).Free();

  inherited Destroy();
end;

function TLapeTools_ScriptParser.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
var
  Path: lpString;
  Declaration: TDeclaration_Include;
  i: Int32;
begin
  if (not Sender.InPeek) and (not InIgnore()) and (FStack.Count = 0) then
    case LowerCase(Directive) of
      'define':
        if (psParseIncludes in FSettings) then
        begin
          i := System.Pos(':=', Argument);

          if (LowerCase(Trim(Copy(Argument, 1, i - 1))) = 'mainfile') then
          begin
            Path := FindFile(Trim(Copy(Argument, i + 2, Length(Argument) - i)));

            if FileExists(Path) then
              Cache.Get(TLapeTools_CachedMainFile, Self, Path).AddToParser(Self);
          end;
        end;

      'i', 'include', 'include_once':
        begin
          Path := FindFile(Argument);

          if FileExists(Path) then
          begin
            Declaration := TDeclaration_Include.Create(Self, True);
            Declaration.Text := ExtractFileName(Declaration.Text);
            Declaration.Path := Path;

            FMap.Add(Declaration.Text, Declaration);

            if (psParseIncludes in FSettings) and (FIncludes.IndexOf(Path) = -1) then
              Cache.Get(TLapeTools_CachedInclude, Self, Path).AddToParser(Self);
          end;

          Exit(True);
        end;
    end;

  Result := inherited HandleDirective(Sender, Directive, Argument);
end;

destructor TLapeTools_ScriptParser.Destroy;
var
  i: Int32;
begin
  for i := 0 to High(CachedFiles) do
    CachedFiles[i].DecRef();

  inherited Destroy();
end;

initialization
  Cache := TLapeTools_Cache.Create();

finalization
  Cache.Free();

end.


