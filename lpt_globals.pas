unit lpt_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

var
  LapeTools_Images: TImageList;

function SingleLine(const S: String): String;

implementation

uses
  LResources;

function SingleLine(const S: String): String;
begin
  Result := S;

  while (Pos(LineEnding, Result) > 0) do
    Result := StringReplace(Result, LineEnding, #32, [rfReplaceAll]);
  while (Pos(#9, Result) > 0) do
    Result := StringReplace(Result, #9, #32, [rfReplaceAll]);
  while (Pos(#32#32, Result) > 0) do
    Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
end;

initialization
  {$i images.lrs}

  LapeTools_Images := TImageList.Create(nil);

  with LapeTools_Images do
  begin
    AddLazarusResource('ce_const');
    AddLazarusResource('ce_variable');
    AddLazarusResource('ce_type');
    AddLazarusResource('ce_procedure');
    AddLazarusResource('ce_function');
    AddLazarusResource('ce_property');
    AddLazarusResource('ce_helper');
    AddLazarusResource('ce_unit');
  end;

finalization
  LapeTools_Images.Free();

end.

