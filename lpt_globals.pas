unit lpt_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  lpt_parser;

var
  LapeTools_Images: TImageList;

function SingleLine(const S: String): String; inline;
function GetImage(Declaration: TDeclaration): Int32; inline;

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

function GetImage(Declaration: TDeclaration): Int32;
begin
  if (Declaration is TDeclaration_Constant) then
    Exit(0);
  if (Declaration is TDeclaration_Variable) then
    Exit(1);
  if (Declaration is TDeclaration_Type) then
    Exit(2);
   if (Declaration is TDeclaration_Label) then
    Exit(3);
  if (Declaration is TDeclaration_Include) then
    Exit(4);
  if (Declaration is TDeclaration_Method) then
    with (Declaration as TDeclaration_Method) do
      case Header.MethodType of
        mtFunction, mtFunctionOfObject: Exit(5);
        mtProcedure, mtProcedureOfObject: Exit(6);
      end;

  Exit(-1);
end;

initialization
  {$i images.lrs}

  LapeTools_Images := TImageList.Create(nil);

  with LapeTools_Images do
  begin
    AddLazarusResource('ce_const');
    AddLazarusResource('ce_variable');
    AddLazarusResource('ce_type');
    AddLazarusResource('ce_helper');
    AddLazarusResource('ce_unit');
    AddLazarusResource('ce_function');
    AddLazarusResource('ce_procedure');
  end;

finalization
  LapeTools_Images.Free();

end.

