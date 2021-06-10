open Datatypes
open Utils

val scrollMode = Unsafe.scrollModeSource "scrollMode" SMNormal
val ultraCompact = Unsafe.boolSource "ultraCompact" False
val listViewMode = Unsafe.lvmSource "listViewMode" LVMCompact(* LVMTwoLines *)
val markReadMode = Unsafe.mrmSource "markReadMode" MRMOnScroll(* MRMOnScrollEverywhere *)

val getScrollMode =
    sm <- get scrollMode;
    return (case sm of
              | SMNormal => "normal"
              | SMQuick => "quick"
              | SMImmediate => "immediate")

val apiUrl = "https://github.com/bazqux/bazqux-api"
fun apiLink t = Utils.hrefLink t apiUrl

val msgDivId = Unsafe.id "msgDivId"
val topId = Unsafe.id "topId"
val snapHeightId = Unsafe.id "snapHeightId"

val articleFontSize = Unsafe.intStorageSource "articleFontSize" 0
val readerFontSize = Unsafe.intStorageSource "readerFontSize" 5
val themeId = Unsafe.stringStorageSource "themeId" ""
val darkThemeId = Unsafe.stringStorageSource "darkThemeId" "night"
val fontFamilyId = Unsafe.stringStorageSource "fontFamilyId" ""
val lineHeightScalePx = Unsafe.intStorageSource "lineHeightScalePx" 0
val lineHeightScaleFontSize = Unsafe.intStorageSource "lineHeightScaleFontSize" 20
val textWidthScale = Unsafe.intStorageSource "textWidthScale" 0
val leftPanelWidthScale = Unsafe.intStorageSource "leftPanelWidthScale" 0
val mobileMarginScale = Unsafe.intStorageSource "mobileMarginScale" 0
val imageProxy = Unsafe.boolStorageSource "imageProxy" True

datatype feedAlign = FALeft | FACenter
val feedAlignEq : eq feedAlign =
    mkEq (fn a b => case (a,b) of
      | (FALeft, FALeft) => True
      | (FACenter, FACenter) => True
      | _ => False)
ffi feedAlignStorageSource_ jsFunc "unsafeStorageEnumSource"
    : list (feedAlign * string) -> string -> feedAlign -> source feedAlign

val feedAlign = feedAlignStorageSource_
  ((FALeft, "left") :: (FACenter, "center") :: [])
  "feedAlign" FALeft

datatype imagesWidth = IWNormal | IWWide | IWFull
val imagesWidthEq : eq imagesWidth =
    mkEq (fn a b => case (a,b) of
      | (IWNormal, IWNormal) => True
      | (IWWide, IWWide) => True
      | (IWFull, IWFull) => True
      | _ => False)
ffi imagesWidthStorageSource_ jsFunc "unsafeStorageEnumSource"
    : list (imagesWidth * string) -> string -> imagesWidth -> source imagesWidth

val imagesWidth = imagesWidthStorageSource_
  ((IWNormal, "normal") :: (IWWide, "wide") :: (IWFull, "full") :: [])
  "imagesWidth" IWNormal


val fontSizes =
    Css.fontSize0 :: Css.fontSize1 :: Css.fontSize2 ::
    Css.fontSize3 :: Css.fontSize4 :: Css.fontSize5 ::
    Css.fontSize6 :: Css.fontSize7 :: Css.fontSize8 ::
    Css.fontSize9 :: Css.fontSize10 :: Css.fontSize11 ::
    Css.fontSize12 :: Css.fontSize13 :: Css.fontSize14 :: []
val readerFontSizeClass =
    b <- signal readerFontSize;
    return (maybe null Js.fixFontSize (List.nth fontSizes b))
val articleFontSizeClass =
    b <- signal readerFontSize;
    m <- signal articleFontSize;
    return (maybe null Js.fixFontSize (List.nth fontSizes (min 14 (b+m))))
val articleFontSizePx : signal int =
    fsc <- articleFontSizeClass;
    return (Js.parseInt (Js.getRuleValueF ("." ^ show fsc) "font-size"))
val readerFontSizePx : signal int =
    fsc <- readerFontSizeClass;
    return (Js.parseInt (Js.getRuleValueF ("." ^ show fsc) "font-size"))
val feedAlignClass =
    ta <- signal feedAlign;
    return (case ta of
        FALeft => Css.feedAlignLeft
      | FACenter => Css.feedAlignCenter)
val imagesWidthClass =
    ta <- signal imagesWidth;
    return (case ta of
        IWNormal => Css.imagesWidthNormal
      | IWWide => Css.imagesWidthWide
      | IWFull => Css.imagesWidthFull)
val imageProxyClass =
    p <- signal imageProxy;
    return (if p then Css.proxyEnabled else Css.proxyDisabled)

fun lineHeight' p fs curFs =
    float (floor ((float p / float fs + 1.5) * float curFs)) / float curFs

val lineHeight =
    p <- signal lineHeightScalePx;
    f <- signal lineHeightScaleFontSize;
    curFs <- articleFontSizePx;
    return (lineHeight' p f curFs)

val fixLineHeightScale =
    lh <- current lineHeight;
    fs <- current articleFontSizePx;
    lhfs <- get lineHeightScaleFontSize;
    when (lhfs <> fs)
         (set lineHeightScaleFontSize fs;
          p <- get lineHeightScalePx;
          when (p <> 0)
               (debug ("lh = " ^ show lh ^ "; round p = " ^ show (round ((lh - 1.5) * float fs)));
                let fun find p =
                        lh' <- current lineHeight;
                        if lineHeight' p fs fs >= lh then
                            debug ("found p = " ^ show p);
                            set lineHeightScalePx p
                        else
                            find (p+1)
                in
                    find (-fs)
                end))
