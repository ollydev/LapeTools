type TFontChar = packed record
  FChar:AnsiChar;
  FWidth,FHeight:Int32;
  loaded, hasShadow:LongBool;
  pts,shadow,bad:TPointArray;
end;;
type TFontset = packed record
  FData: Array of TFontChar;
  SpaceWidth: Int32;
end;;
type TCompareRules = packed record
  Color, ColorMaxDiff: Int32;
  UseShadow: LongBool;
  ShadowMaxValue:Int32;
  Threshold: Int32;
  ThreshInv: LongBool;
end;;
type TSimpleOCR = packed record
  IsLoaded: LongBool;
  FontData: TFontSet;
  ClientID: TTarget_Exported;
  Client:   T2DIntArray;
  __maxShadowAvg: Int32;
  __debugging: LongBool;
end;;
procedure TFontSet.Load(Font:AnsiString; Space:Int32=4); begin end;
procedure TFontSet.Free(); begin end;
procedure TSimpleOCR.Init(FontPath:TFontSet; AClient:TTarget_Exported=ExportImageTarget()); native; begin end;
procedure TSimpleOCR.Init(Font:AnsiString; SpaceWidth:Int32=4; AClient:TTarget_Exported=ExportImageTarget()); overload; native; begin end;
procedure TSimpleOCR.SetFont(FontPath:TFontSet); native; begin end;
procedure TSimpleOCR.SetFont(Font:AnsiString; SpaceWidth:Int32=4); overload; native; begin end;
procedure TSimpleOCR.Free(); native; begin end;
function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; native; begin end;
function TSimpleOCR.RecognizeEx(AClient:T2DIntArray; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; native; begin end;
