val appearanceBoxClass : string
val newAppearanceDialog : transaction Popups.popup
val handleLayoutChange : transaction {}

val setRoundedVariables : string -> string -> float -> transaction {}
val withLayoutChange : transaction {} -> transaction {}
val withNoTransitions : transaction {} -> transaction {}
val decArticleFontSize : transaction {}
val incArticleFontSize : transaction {}
val decReaderFontSize : transaction {}
val incReaderFontSize : transaction {}
val updateImageSizes : transaction {}

con font
  = { Id : string
    , FontName : string
    , ParagraphFont : string
    , HeaderFont : option string
    , MonospaceFont : option string
    , SupportsSuper : bool
    , SupportsSub : bool
    }

val fontAndLineHeight : signal (font * float)
val fontCSS : xbody
