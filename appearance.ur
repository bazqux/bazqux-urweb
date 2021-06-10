open Articles
structure P = Popups

val handleLayoutChange =
    Pager.invalidate pager;
    (getmw ()).UpdateFeedMark;
    (getmw ()).RestoreSelectedUIMPosition
fun withLayoutChange act =
    (getmw ()).SaveSelectedUIMPosition;
    act;
    handleLayoutChange
fun changeScale src f =
    s <- get src;
    let val s' = f s
    in
        when (s' <> s) (withLayoutChange (set src s'))
    end
val changeReaderFontSize = changeScale Settings.readerFontSize
val changeArticleFontSize = changeScale Settings.articleFontSize
fun decScale n = max 0 (n-1)
val maxArticleFontSize = 6
val maxReaderFontSize = 8
val decArticleFontSize = changeArticleFontSize decScale
val incArticleFontSize = changeArticleFontSize (fn n => min maxArticleFontSize (n+1))
val decReaderFontSize = changeReaderFontSize decScale
val incReaderFontSize = changeReaderFontSize (fn n => min maxReaderFontSize (n+1))

con font
  = { Id : string
    , FontName : string
    , ParagraphFont : string
    , HeaderFont : option string
    , MonospaceFont : option string
    , SupportsSuper : bool
    , SupportsSub : bool
    }

con theme
  = { Id : string
    , ThemeName : string
    , BackgroundColor : int * int * int
    , InterpolateColor : int * int * int
    , TextColor : int * int * int
    , LinkColor : string
    , ErrorColor : string
    , MarkColor : string
    , InvertedButtonGradients : bool
    , MinOpacity : float
    , LandingPageButtonsOpacity : float
(*     , FaviconBackground : string *)
    }

val (defaultTheme, defaultDarkTheme, themes) =
    let fun theme i n bg ic t l e m ig minOp bop : theme =
            { Id = i
            , ThemeName = n
            , BackgroundColor = bg
            , InterpolateColor = ic
            , TextColor = t
            , LinkColor = l
            , ErrorColor = e
            , MarkColor = m
            , InvertedButtonGradients = ig
            , MinOpacity = minOp
            , LandingPageButtonsOpacity = bop
            }
        val def =
            theme "" "Default" (255,255,255) (255,255,255) (0,0,0) "#06c" "#933" "yellow" False 0.0 1.0
        val dark =
            theme "night" "Night" (0, 0, 0) (21, 21, 21) (176, 176, 176)
           (* "rgb(72, 160, 200)" *)"rgb(75, 168, 210)" "#b33" "darkgreen" True 0.1(*0.07*) 0.85
    in
        (def, dark, def
        :: theme "sepia" "Sepia" (248, 241, 227) (248, 241, 227) (79, 50, 28)
           "rgb(209, 150, 0)" "#933" "wheat" False 0.0 1.0
        :: theme "gray" "Gray" (74, 74, 77) (76, 76, 79) (215, 215, 216)
           "rgb(90, 200, 250)" "#c44" "darkolivegreen" True 0.05 1.0
        :: dark
        :: [])
    end

fun findTheme def tid = lookupDefault def (fn t => t.Id = tid) themes

val (defaultFont, fonts) =
    let fun font id n ff h m sup sub : font =
            { Id = id, FontName = n, ParagraphFont = ff, HeaderFont = h, MonospaceFont = m, SupportsSuper = sup, SupportsSub = sub }
        val defaultSans = "sans-serif"
        fun withDefaultSans f = f ^ ", " ^ defaultSans
        val defaultMono = "monospace, monospace"
            (* два раза, чтобы браузеры не уменьшали высоту *)
            (* "Menlo, Monaco, 'Courier New', monospace" *)
        fun serif id n h =
            font id n (id ^ ", serif") (Option.mp withDefaultSans h)
        fun sans  id n = font id n (withDefaultSans id) None
        val firaMono = Some "Fira Mono"
        fun ibmPlex x =
            x ^ ", IBMPlexDevanagari, IBMPlexSansHebrew, IBMPlexThai"
        val def =
            font "Charter"       "Charter"
                 (if Js.isWin () then
                      "ITC Charter, serif"
                  else
                      "PT ITC Charter, serif")
                 (Some (withDefaultSans "FF Kievit")) firaMono
                 (Js.isWin ()) False
                 (* у PT ITC Charter/PT Serif super есть только для 123,
                    у ITC Charter super поддерживается для всех чисел
                    у PT Serif sub есть только для 1234
                  *)
    in
        (def,
(*         :: font  "github"        "Apple system" *)
(*            "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"" *)
           (* ^ хорошо только на мобиле на небольшом размере шрифта,
                у Twitter
                  system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Ubuntu, "Helvetica Neue", sans-serif;
            *)
           def
        :: serif "PT Iowan Old Style"
                                 "Iowan" (Some "FF Tisa Sans") firaMono True False
        :: serif "PT Serif"      "PT Serif" (Some "PT Sans") (Some "PT Mono") False False
        :: sans  "FF Kievit"     "Kievit" firaMono True True
        :: sans  "FF Tisa Sans"  "Tisa Sans" firaMono True True
        :: sans  "FiraGO"        "FiraGO" firaMono True True
(*         :: sans  "SourceSansPro" "Source Sans" (Some "SourceCodePro") *)
(*         :: font  "IBMPlexSerif" "IBM Plex Serif" (ibmPlex "IBMPlexSerif" ^ ", serif") (Some (ibmPlex "IBMPlexSans")) (Some "IBMPlexMono") *)
        :: font  "system" "System" defaultSans None None False False
        :: [])
    end

fun css x =
    Unsafe.toXml ("<style>" ^ Js.strReplace "</" "<\\/" x ^ "</style>")
    (* заменяем потенциальный </style> *)
fun cssRule selector property value =
    selector ^ "{\n  " ^ property ^ ": " ^ value ^ "\n}\n"
fun setRoundedVariables sel p lh =
    let val fs = Js.parseInt p
        fun r x = show (round (x * float fs)) ^ "px"
        fun r2 x = show (round (x * float fs / 2.0) * 2) ^ "px"
        val s = Js.setVariableTo sel
    in
        s "--emr_015" (r 0.15);
        s "--emr_025" (r 0.25);
        s "--emr_03"  (r 0.3);
        s "--emr_05"  (r 0.5);
        s "--emr_075" (r 0.75);
        s "--emr_08"  (r 0.8);
        s "--emr_105" (r 1.05);
        s "--emr_15"  (r 1.5);
        s "--emr_34"  (r 3.4);
        s "--emr2_12"  (r2 1.2);
        s "--emr2_138" (r2 1.38);
        s "--emr2_15"  (r2 1.5);
        s "--lhr" (r lh);
        s "--lhr_05"  (r (lh * 0.5));
        s "--lhr_025" (r (lh * 0.25))
    end

fun showRgbColor (c : int * int * int) = "rgb" ^ show c
fun showRgbaColor (c : int * int * int * float) = "rgba" ^ show c
(* fun showColor (r,g,b) = "rgb(" *)

fun interpolate x (a,b,c) (aa,bb,cc) =
    let fun i x a b =
            round ((1.0 - float x / 255.0)*float a + (float x / 255.0)*float b)
    in
        (i x a aa, i x b bb, i x c cc)
    end

val appearanceBoxClass = ".appearanceBox.smallDialogBox"

fun setInterpolatedColors m fg bg =
    let val setDefault = Js.setMediaVariableTo m appearanceBoxClass
        val black = (0,0,0)
        val white = (255,255,255)
        fun s n c =
            Js.setMediaVariable m ("--c" ^ n) (showRgbColor (interpolate c fg bg));
            setDefault ("--c" ^ n) (showRgbColor (interpolate c black white))
            (* можно еще "inital" ставить *)
        fun rgba (r,g,b) a = (r,g,b,a)
    in
        Js.setMediaVariable m "--c000_03" (showRgbaColor (rgba fg 0.3));
        setDefault "--c000_03" (showRgbaColor (rgba black 0.3));
        s "333" 0x33;
        s "444" 0x44;
        s "666" 0x66;
        s "707070" 0x70;
        s "777" 0x77;
        s "888" 0x88;
        s "999" 0x99;
        s "a0a0a0" 0xA0;
        s "aaa" 0xAA;
        s "ccc" 0xCC;
        s "ddd" 0xDD;
        s "e8e8e8" 0xE8;
        s "eaeaea" 0xEA;
        s "ececec" 0xEC;
        s "eee" 0xEE;
        s "f0f0f0" 0xF0;
        s "f5f5f5" 0xF5;
        s "f8f8f8" 0xF8;
        s "fafafa" 0xFA;
        s "fcfcfc" 0xFC
    end

val fontAndLineHeight =
    ffid <- signal Settings.fontFamilyId;
    lh <- Settings.lineHeight;
    return (lookupDefault defaultFont (fn f => f.Id = ffid) fonts, lh)

val baseTextWidth = 37 (* em *)
fun leftPanelWidth s = 270 + s
fun leftPanelWidthPx s = show (leftPanelWidth s) ^ "px"
val imageResolutions = (320, 640) :: (640, 1280) :: (960, 1440) :: (1280, 1920) :: (1440, 1920) :: (1920, 2560) :: (2560, 2560) :: []


fun updateImageSizes' imageProxy textWidthScale imagesWidth articleFontSizeClass =
    if not imageProxy then
        Js.setImageProxy ""
    else
        vw <- Js.viewportWidth;
        dpi <- Js.windowDevicePixelRatio;
        (* Chrome/FF увеличивают devicePixelRatio при zoom, Safari нет *)
        as <- Js.getRuleValue ("." ^ show articleFontSizeClass) "font-size";
        let val maxSize =
                if imagesWidth = Settings.IWFull || vw <= 900.0 then
                    vw
                else
                    float ((baseTextWidth + textWidthScale) * Js.parseInt as)
                    * (if imagesWidth = Settings.IWWide then 4.0/3.0 else 1.0)
            fun findSize width l rs = case rs of
                  | [] => l
                  | (w,h) :: rs' =>
                    if round (width * dpi) = w || round (width * dpi * 1.05) < w
                    then
                        (w, h)
                    else
                        findSize width (w, h) rs'
            val (w, h) = findSize maxSize (0,0) imageResolutions
        in
            Js.setImageProxy (show w ^ "x" ^ show h)
        end

val updateImageSizes =
    ip <- get Settings.imageProxy;
    s <- get Settings.textWidthScale;
    iw <- get Settings.imagesWidth;
    afs <- current Settings.articleFontSizeClass;
    updateImageSizes' ip s iw afs

val fontCSS = <xml>
    {dyn_ (
        (f, lh) <- fontAndLineHeight;
        rfs <- Settings.readerFontSizeClass;
        afs <- Settings.articleFontSizeClass;
        return (activeCode (
            Js.setVariable "--header-line-height" (show lh);
            Js.setVariable "--font-size-sup-sub"
                (show (min 0.8 ((lh / 1.5) * 0.8)) ^ "em");
            Js.setVariable "--header-font-family"
                (Option.get f.ParagraphFont f.HeaderFont);
            Js.setVariable "--paragraph-font-family" f.ParagraphFont;
            Js.setVariable "--monospace-font-family"
                (case f.MonospaceFont of
                   | None => "monospace, monospace"
                   | Some f => f ^ ", monospace");
            rs <- Js.getRuleValue ("." ^ show rfs) "font-size";
            Js.setVariable "--reader-font-size" rs;
            setRoundedVariables ("." ^ show rfs) rs lh;
            as <- Js.getRuleValue ("." ^ show afs) "font-size";
            Js.setVariable "--article-font-size" as;
            setRoundedVariables ("." ^ show afs) as lh;
            (* для baseline compensation нужен и шрифт и размер *)
            Js.updateBaselineCompensation rfs afs)))}
    {dyn_ (
        tid <- signal Settings.themeId;
        dtid <- signal Settings.darkThemeId;
        let fun setThemeColors m t =
                Js.setMediaVariable m "--background-color" (showRgbColor t.BackgroundColor);
                Js.setMediaVariable m "--text-color" (showRgbColor t.TextColor);
                Js.setMediaVariable m "--interpolate-color" (showRgbColor t.InterpolateColor);
                Js.setMediaVariable m "--link-color" t.LinkColor;
                Js.setMediaVariable m "--error-color" t.ErrorColor;
                Js.setMediaVariable m "--mark-color" t.MarkColor;
                Js.setMediaVariable m "--button-gradient-degrees"
                    (if t.InvertedButtonGradients then "0deg" else "180deg");
                Js.setMediaVariable m "--min-opacity" (show t.MinOpacity);
                Js.setMediaVariable m "--landingPage-buttons-opacity"
                    (show t.LandingPageButtonsOpacity);
                setInterpolatedColors m t.TextColor t.InterpolateColor;
                Js.updateButtonsColor m (showRgbColor t.TextColor)
        in
        return (activeCode (
          setThemeColors "screen" (findTheme defaultTheme tid);
          setThemeColors "screen and (prefers-color-scheme: dark)"
              (findTheme defaultDarkTheme dtid)))
        end)}
    {dyn_ (
        ip <- signal Settings.imageProxy;
        s <- signal Settings.textWidthScale;
        lpw <- signal Settings.leftPanelWidthScale;
        m <- signal Settings.mobileMarginScale;
        iw <- signal Settings.imagesWidth;
        afs <- Settings.articleFontSizeClass;
        return (activeXml (
            Js.setVariable "--text-width" (show (baseTextWidth + s) ^ "em");
            Js.setVariable "--mobile-margin" (show (0.75 + 0.25 * float m) ^ "em");
            Js.setVariable "--left-panel-width" (leftPanelWidthPx lpw);
            updateImageSizes' ip s iw afs;
            return <xml/>)))}
    </xml>

fun withNoTransitions a =
    set noTransitions True;
    a;
    set noTransitions False

fun withNoScrollbars a =
    Js.addClass "html" Css.noScrollbars;
    a;
    Js.removeClass "html" Css.noScrollbars

val newAppearanceDialog =
    let fun line t c hint x =
            <xml><div class={classes c appearanceLine}>
              <div class="appearanceTitle">
                {case hint of
                   | Some hs =>
                     dyn_ (h <- hs;
                           return <xml><div title={h}>{[t]}</div></xml>)
                   | _ => txt t}
              </div>{x}</div></xml>
        fun font f = <xml>
          <li onclick={fn _ => withLayoutChange
                                   (set Settings.fontFamilyId f.Id)}
              style={Unsafe.toStyle ("font-family: " ^ (
                     (if f.Id <> "system" then
                         Js.strReplace f.Id (f.Id ^ " subset")
                     else
                         id)
                     f.ParagraphFont) ^ " !important")}>
          {iconCheckIfS ((cf,_) <- fontAndLineHeight; return (cf.Id = f.Id))}
          <span class={Css.buttonText}>{[f.FontName]}</span>
        </li></xml>
        fun setTheme themeId t =
            withNoTransitions (withNoScrollbars (set themeId t.Id))
        fun theme themeId t = <xml>
          <li onclick={fn _ => setTheme themeId t}
              style={Unsafe.toStyle ("background-color: " ^ showRgbColor t.BackgroundColor
                  ^ "; color: " ^ showRgbColor t.TextColor ^ "; border-bottom: 1px solid " ^ showRgbColor (interpolate 0xDD t.TextColor t.InterpolateColor))}>
          {iconCheckIfS (eqS themeId t.Id)}
          <span class={Css.buttonText}>{[t.ThemeName]}</span>
        </li></xml>
        fun buttonD s c name hint act =
            disabledIf s (textButton'' c name hint act)
        fun sButton cls (name:string) (title:string) click : xbody =
            (* aналог textButton'', не меняющий ширины,
               если текст становится жирным *)
            <xml><span onclick={fn _ => click} class={classes cls Css.textButton} title={title}><span class="boldPlaceholder">{[name]}</span>{[name]}</span></xml>
        fun toggleButton [a] (_ : eq a) (s : source a) (v : a) c name hint =
            selectedIf (eqS s v) (sButton c name hint
              (v0 <- get s;
               when (v0 <> v)
                    (withLayoutChange (set s v))))
        val imagesWidthButton = toggleButton Settings.imagesWidth
        val feedAlignButton = toggleButton Settings.feedAlign
        val imageProxyButton = toggleButton Settings.imageProxy
        fun scaleButtons s step minScale maxScale = <xml>
            {buttonD (mapS (fn l => l <= minScale) s) Css.buttonLeft
                     "−" "Decrease" (changeScale s (plus (-step)))}
            {buttonD (eqS s 0) Css.buttonMiddle
                     "×" "Reset" (changeScale s (const 0))}
            {buttonD (mapS (fn l => l >= maxScale) s) Css.buttonRight
                     "+" "Increase" (changeScale s (plus step))}
        </xml>
        fun changeLineHeight f =
            Settings.fixLineHeightScale;
            changeScale Settings.lineHeightScalePx f
        val lineHeightScaleButtons = <xml>
            {buttonD (Monad.mp (fn l => l <= 1.0) Settings.lineHeight)
                     Css.buttonLeft
                     "−" "Decrease" (changeLineHeight (plus (-1)))}
            {buttonD (eqS Settings.lineHeightScalePx 0)
                     Css.buttonMiddle
                     "×" "Reset" (changeLineHeight (const 0))}
            {buttonD (Monad.mp (fn l => l >= 2.0) Settings.lineHeight)
                     Css.buttonRight
                     "+" "Increase" (changeLineHeight (plus 1))}
        </xml>
    in
    P.newBoxC Css.appearanceBox "Appearance" <xml>
      Theme
      <div class="themesList lightTheme">
        {List.mapX (theme Settings.themeId) themes}
      </div>
      <div class="themesList darkTheme">
        {List.mapX (theme Settings.darkThemeId) themes}
      </div>
      {line "Reader font size" null
        (Some (p <- Settings.readerFontSizePx;
               return ("Reader font size: " ^ show p ^ "px")))
        <xml>
          {buttonD (eqS Settings.readerFontSize 0) Css.buttonLeft
            "a"
            "Decrease whole reader font size. \nKeyboard shortcut: '_'"
            decReaderFontSize}
          {buttonD (eqS Settings.readerFontSize maxReaderFontSize) Css.buttonRight
            "A"
            "Increase whole reader font size. \nKeyboard shortcut: '+'"
            incReaderFontSize}
        </xml>}
      {line "Article font size" null
        (Some (p <- Settings.articleFontSizePx;
               return ("Article font size: " ^ show p ^ "px")))
        <xml>
          {buttonD (eqS Settings.articleFontSize 0) Css.buttonLeft
             "a"
             "Decrease article font size. \nKeyboard shortcut: '-'"
             decArticleFontSize}
          {buttonD (eqS Settings.articleFontSize maxArticleFontSize) Css.buttonRight
             "A"
             "Increase article font size. \nKeyboard shortcut: '='"
             incArticleFontSize}
        </xml>}
      {line "Margins" Css.hideInNormalView None
        (scaleButtons Settings.mobileMarginScale 1 (-3) 12)}
      {line "Text width" Css.hideInMobileView
        (Some (em <- Monad.mp (plus baseTextWidth)
                              (signal Settings.textWidthScale);
               afs <- Settings.articleFontSizePx;
               return ("Text width: " ^ show em ^ "em (" ^ show (em * afs) ^ "px)")))
        (scaleButtons Settings.textWidthScale 1 (-10) 1000000)}
      {line "Images width" Css.hideInMobileView None <xml>
        {imagesWidthButton Settings.IWNormal Css.buttonLeft
                           "same" "Same width as text"}
(*         {imagesWidthButton Settings.IWWide "wider" "Wider than the text"} *)
        {imagesWidthButton Settings.IWFull Css.buttonRight
                           "full" "Full available width"}
      </xml>}
      {line "Feed align" (classes Css.hideInMobileView Css.disabledInFullscreen) None <xml>
        {feedAlignButton Settings.FALeft Css.buttonLeft
                         "left" "Articles are aligned to the left side"}
        {feedAlignButton Settings.FACenter Css.buttonRight
                         "center" "Articles are shown in the center"}
      </xml>}
      {line "Line height" null
        (Some (lh <- Settings.lineHeight;
               afs <- Settings.articleFontSizePx;
               return ("Line height: " ^ show (floor (float afs * lh)) ^ "px (" ^ Js.toFixed lh 3 ^ ")")))
        lineHeightScaleButtons}
(*       {line <xml>List view...</xml>} *)
      Font
      <div class="fontsList">
        {List.mapX font fonts}
      </div>
      {line "Image proxy" null None <xml>
        {imageProxyButton True Css.buttonLeft "on" "Proxy external images through BazQux Reader servers to improve speed and save traffic (overlarge images are scaled down) as well as reduce tracking (you only connect to BazQux Reader servers, other sites do not know what images you’ve accessed)"}
        {imageProxyButton False Css.buttonRight "off" "Disable proxying of external images through BazQux Reader servers"}
      </xml>}
      {line "Left panel" Css.hideWhenLeftPanelIsNarrow
        (Some (s <- signal Settings.leftPanelWidthScale;
               return ("Left panel width: " ^ leftPanelWidthPx s)))
        (scaleButtons Settings.leftPanelWidthScale 5
          (190 - leftPanelWidth 0)
          (1000 - leftPanelWidth 0))}
      <div class="appearanceLine">
        All appearance settings<br/>
        are saved per browser.
      </div>
    </xml>
    end
