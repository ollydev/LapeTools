'type TFFI_ABI = (FFI_CDECL = 1, FFI_STDCALL = 2, FFI_THISCALL = 3, FFI_FASTCALL = 4, FFI_MS_CDECL = 5,FFI_PASCAL = 6, FFI_REGISTER = 7);' + LineEnding +
'type TLibHandle = TLibHandle;' + LineEnding +
{$IFDEF WINDOWS}
'var SharedSuffix: String = ' + #39 + '.dll' + #39 + ';' + LineEnding +
{$ELSE}
'var SharedSuffix: String = ' + #39 + '.so' + #39 + ';' + LineEnding +
{$ENDIF}
'function LoadLibrary(const Name: string): TLibHandle; begin end;' + LineEnding +
'function GetProcAddress(Lib: TlibHandle; const ProcName: string): ConstPointer; begin end;' + LineEnding +
'function FreeLibrary(Lib: TLibHandle): EvalBool; begin end;' + LineEnding;
