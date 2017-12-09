program debugger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cmem, cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, SysUtils,
  debugger_main;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Initialize();
  Application.CreateForm(TDebugForm, DebugForm);
  Application.Run();

  {$IF DECLARED(SetHeapTraceOutput)}
  if (FileExists('leakage.trc')) then
    DeleteFile('leakage.trc');

  SetHeapTraceOutput('leakage.trc');
  {$ENDIF}
end.

