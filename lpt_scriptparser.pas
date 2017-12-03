unit lpt_scriptparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lptypes, lpparser, lpt_parser;

type
  TLapeTools_CachedInclude = class(TLapeTools_Parser)
  public
    RefCount: Int32;

    function Updated: Boolean;
  end;

  TLapeTools_CachedIncludes = specialize TLapeList<TLapeTools_CachedInclude>;

  TLapeTools_IncludeCache = class
  protected
    FIncludes: TLapeTools_CachedIncludes;
  public
    function Get(Sender: TLapeTools_Parser; FilePath: lpString): TLapeTools_CachedInclude;

    constructor Create;
    destructor Destroy; override;
  end;

  TLapeTools_ScriptParser = class(TLapeTools_Parser)
  protected
    FCachedIncludes: TLapeTools_CachedIncludes;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; override;
  public
    constructor Create(ADoc: lpString; AFilePath: lpString; ACaret: Int32); overload;
    constructor Create(AFilePath: lpString); overload;

    destructor Destroy; override;
  end;

implementation

uses
  DateUtils;

var
  IncludeCache: TLapeTools_IncludeCache;

function TLapeTools_CachedInclude.Updated: Boolean;
var
  i: Int32;
begin
  if (SysUtils.FileAge(Self.FilePath) <> Self.FileAge) then
    Exit(True);

  for i := 0 to FIncludes.Count - 1 do
    if SysUtils.FileAge(FIncludes[i]) <> Int32(FIncludes.Objects[i]) then
      Exit(True);

  Exit(False);
end;

function TLapeTools_IncludeCache.Get(Sender: TLapeTools_Parser;FilePath: lpString): TLapeTools_CachedInclude;
var
  i: Int32;
begin
  for i := FIncludes.Count - 1 downto 0 do
    if (FIncludes[i].Updated) and (FIncludes[i].RefCount = 0) then
    begin
      WriteLn('Deleting old include "', FilePath, '"');

      FIncludes.Delete(i).Free();
    end;

  for i := FIncludes.Count - 1 downto 0 do
    if (FIncludes[i].FilePath = FilePath) and (not FIncludes[i].Updated) then
    begin
      Result := FIncludes[i];
      Result.RefCount := Result.RefCount + 1;

      Exit;
    end;

  WriteLn('Caching include "', FilePath, '"');

  Result := TLapeTools_CachedInclude.Create(FilePath);
  Result.Paths.AddStrings(Sender.Paths);
  Result.Parse();
  Result.RefCount := Result.RefCount + 1;

  for i := 0 to Result.Includes.Count - 1 do
    Result.Includes.Objects[i] := TObject(FileAge(Result.Includes[i]));

  FIncludes.Add(Result);
end;

constructor TLapeTools_IncludeCache.Create;
begin
  FIncludes := TLapeTools_CachedIncludes.Create(nil, dupAccept, False);
end;

destructor TLapeTools_IncludeCache.Destroy;
begin
  while (FIncludes.Count > 0) do
    FIncludes.Delete(0).Free();

  FIncludes.Free();
end;

function TLapeTools_ScriptParser.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
var
  Path: lpString;
  Declaration: TDeclaration_Include;
  i: Int32;
begin
  if (not Sender.InPeek) and (not InIgnore()) and (FStack.Count = 0) then
    case LowerCase(Directive) of
      'i', 'include', 'include_once':
        begin
          Path := FindFile(Argument);

          if FileExists(Path) then
          begin
            Declaration := TDeclaration_Include.Create(Self, True);
            Declaration.Text := ExtractFileName(Declaration.Text);
            Declaration.Path := Path;

            FMap.Add(Declaration.Text, Declaration);

            if (FParseIncludes) and (FIncludes.IndexOf(Path) = -1) then
            begin
              FIncludes.Add(Path);

              with FCachedIncludes[FCachedIncludes.Add(IncludeCache.Get(Self, Path))].Map.ExportToArrays() do
                for i := 0 to High(Keys) do
                  FMap.Add(Keys[i], Items[i]);
            end;
          end;

          Exit(True);
        end;
    end;

  Result := inherited HandleDirective(Sender, Directive, Argument);
end;

constructor TLapeTools_ScriptParser.Create(ADoc: lpString; AFilePath: lpString; ACaret: Int32);
begin
  inherited Create(ADoc, AFilePath, ACaret);

  FCachedIncludes := TLapeTools_CachedIncludes.Create(nil, dupAccept, False);
end;

constructor TLapeTools_ScriptParser.Create(AFilePath: lpString);
begin
  inherited Create(FilePath);

  FCachedIncludes := TLapeTools_CachedIncludes.Create(nil, dupAccept, False);
end;

destructor TLapeTools_ScriptParser.Destroy;
begin
  while (FCachedIncludes.Count > 0) do
  begin
    FCachedIncludes[0].RefCount := FCachedIncludes[0].RefCount - 1;
    FCachedIncludes.Delete(0);
  end;

  FCachedIncludes.Free();

  inherited Destroy();
end;

initialization
  IncludeCache := TLapeTools_IncludeCache.Create();

finalization
  IncludeCache.Free();

end.

