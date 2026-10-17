-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad (unless, void)
import           Data.Maybe (isJust)
import           Prelude hiding ((!!))
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.String as MS
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet)
----------------------------------------------------------------------------
-- | A file picked by the user
data FileInfo
  = FileInfo
  { fileName :: MisoString
  , fileType :: MisoString
  , fileSize :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Where we are in reading a file
data Status
  = Idle
  | Reading FileInfo
  | Loaded FileInfo MisoString
  | Failed MisoString
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Model
data Model
  = Model
  { _status :: Status
  , _dragging :: Bool
  } deriving (Show, Eq)
----------------------------------------------------------------------------
progress :: Lens Model Status
progress = lens _status $ \r x -> r { _status = x }
----------------------------------------------------------------------------
dragging :: Lens Model Bool
dragging = lens _dragging $ \r x -> r { _dragging = x }
----------------------------------------------------------------------------
-- | Action
data Action
  = ReadFile DOMRef
  | StartRead FileInfo
  | SetContent FileInfo MisoString
  | ReadFailed MisoString
  | DragEnter
  | DragLeave
  | Clear
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
main :: IO ()
main = startApp (defaultEvents <> dragEvents) app
----------------------------------------------------------------------------
app :: App Model Action
app = (component (Model Idle False) updateModel viewModel)
  { styles = [ Sheet sheet ]
  }
----------------------------------------------------------------------------
-- | Larger files are refused, rendering them would stall the page
maxBytes :: Int
maxBytes = 1024 * 1024
----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  ReadFile input -> do
    dragging .= False
    withSink $ \sink -> do
      -- Index the FileList directly: 'files' expects a real Array, and a
      -- FileList is only array-like
      file <- (!! 0) =<< input ! "files"
      noFile <- isUndefined file
      unless noFile $ do
        info <- fileInfo file
        case checkFile info of
          Just err -> sink (ReadFailed err)
          Nothing -> do
            sink (StartRead info)
            reader <- newFileReader
            setField reader "onload" =<< asyncCallback (do
              result <- fromJSValUnchecked =<< reader ! "result"
              sink (SetContent info result))
            setField reader "onerror" =<< asyncCallback
              (sink (ReadFailed ("Could not read " <> fileName info <> ".")))
            void $ reader # "readAsText" $ [file]
      -- Clear the input so picking the same file again fires "change"
      setValue input ""
  StartRead info ->
    progress .= Reading info
  SetContent info content ->
    progress .= Loaded info content
  ReadFailed err ->
    progress .= Failed err
  DragEnter ->
    dragging .= True
  DragLeave ->
    dragging .= False
  Clear ->
    progress .= Idle
----------------------------------------------------------------------------
fileInfo :: JSVal -> IO FileInfo
fileInfo file =
  FileInfo
    <$> (fromJSValUnchecked =<< file ! "name")
    <*> (fromJSValUnchecked =<< file ! "type")
    <*> (fromJSValUnchecked =<< file ! "size")
----------------------------------------------------------------------------
-- | Refuse files that are too large, or obviously not text
checkFile :: FileInfo -> Maybe MisoString
checkFile FileInfo {..}
  | fileSize > maxBytes =
      Just $ fileName <> " is " <> formatBytes fileSize
        <> ". Pick a file under " <> formatBytes maxBytes <> "."
  | any (\p -> isJust (MS.stripPrefix p fileType)) binaryTypes =
      Just $ fileName <> " is " <> fileType <> ", not text."
  | otherwise = Nothing
  where
    binaryTypes = [ "image/", "audio/", "video/", "font/" ]
----------------------------------------------------------------------------
formatBytes :: Int -> MisoString
formatBytes n
  | n < 1024 = ms n <> " B"
  | n < 1024 * 1024 = tenths 1024 <> " KB"
  | otherwise = tenths (1024 * 1024) <> " MB"
  where
    tenths d =
      let t = (n * 10) `div` d
      in ms (t `div` 10) <> "." <> ms (t `mod` 10)
----------------------------------------------------------------------------
lineCount :: MisoString -> Int
lineCount = length . lines . MS.unpack
----------------------------------------------------------------------------
plural :: Int -> MisoString -> MisoString
plural 1 word = "1 " <> word
plural n word = ms n <> " " <> word <> "s"
----------------------------------------------------------------------------
viewModel :: Model -> View context props Model Action
viewModel Model {..} =
  H.main_
  [ P.class_ "page"
  ]
  [ H.header_
    [ P.class_ "header"
    ]
    [ H.h1_ [] [ "🍜 File Reader" ]
    , H.p_
      [ P.class_ "subtitle"
      ]
      [ "Read a text file in the browser with miso's "
      , H.code_ [] [ "FileReader" ]
      , " bindings. Nothing is uploaded."
      ]
    ]
  , H.label_
    [ P.classList_
      [ ("dropzone", True)
      , ("dragging", _dragging)
      ]
    ]
    [ H.input_
      [ P.type_ "file"
      , P.class_ "file-input"
      , E.onChangeWith (const ReadFile)
      , E.onDragEnter DragEnter
      , E.onDragLeave DragLeave
      , E.onDrop defaultOptions DragLeave
      ]
    , H.div_ [ P.class_ "dropzone-icon" ] [ "📄" ]
    , H.p_
      [ P.class_ "dropzone-title"
      ]
      [ "Drop a file here, or "
      , H.span_ [ P.class_ "browse" ] [ "browse" ]
      ]
    , H.p_
      [ P.class_ "dropzone-hint"
      ]
      [ text ("Text files up to " <> formatBytes maxBytes)
      ]
    ]
  , viewStatus _status
  , H.footer_
    [ P.class_ "footer"
    ]
    [ H.a_
      [ P.href_ "https://github.com/haskell-miso/filereader"
      ]
      [ "Source on GitHub"
      ]
    ]
  ]
----------------------------------------------------------------------------
viewStatus :: Status -> View context props Model Action
viewStatus = \case
  Idle ->
    vfrag []
  Reading FileInfo {..} ->
    H.div_
    [ P.class_ "notice"
    ]
    [ text ("Reading " <> fileName <> "…")
    ]
  Failed err ->
    H.div_
    [ P.class_ "notice error"
    , P.role_ "alert"
    ]
    [ text err
    ]
  Loaded FileInfo {..} content ->
    H.section_
    [ P.class_ "card"
    ]
    [ H.div_
      [ P.class_ "card-header"
      ]
      [ H.div_
        [ P.class_ "file-meta"
        ]
        [ H.h2_ [ P.class_ "file-name" ] [ text fileName ]
        , H.p_
          [ P.class_ "file-details"
          ]
          [ text $ MS.intercalate " · " $
              [ fileType | fileType /= mempty ] ++
              [ formatBytes fileSize
              , plural (lineCount content) "line"
              ]
          ]
        ]
      , H.button_
        [ P.class_ "clear"
        , E.onClick Clear
        ]
        [ "Clear"
        ]
      ]
    , if content == mempty
      then H.p_ [ P.class_ "empty" ] [ "This file is empty." ]
      else H.pre_ [ P.class_ "content" ] [ text content ]
    ]
----------------------------------------------------------------------------
sheet :: StyleSheet
sheet =
  CSS.sheet_
  [ CSS.selector_ ":root"
    [ "--bg" =: "#f6f7fb"
    , "--surface" =: "#ffffff"
    , "--text" =: "#1f2330"
    , "--muted" =: "#636a7e"
    , "--border" =: "#dde1ea"
    , "--accent" =: "#4a6bff"
    , "--accent-soft" =: "rgba(74, 107, 255, 0.08)"
    , "--danger" =: "#c4314b"
    , "--danger-soft" =: "rgba(196, 49, 75, 0.08)"
    , "--code-bg" =: "#f3f4f8"
    , "--shadow" =: "0 1px 2px rgba(16, 24, 40, 0.06), 0 4px 16px rgba(16, 24, 40, 0.06)"
    , "color-scheme" =: "light dark"
    ]
  , CSS.media_ (CSS.prefersColorScheme_ "dark")
    [ CSS.rule_ ":root"
      [ "--bg" =: "#12141b"
      , "--surface" =: "#1b1e27"
      , "--text" =: "#e6e8ef"
      , "--muted" =: "#9aa1b5"
      , "--border" =: "#2c303d"
      , "--accent" =: "#7d95ff"
      , "--accent-soft" =: "rgba(125, 149, 255, 0.12)"
      , "--danger" =: "#ff7a8f"
      , "--danger-soft" =: "rgba(255, 122, 143, 0.12)"
      , "--code-bg" =: "#151820"
      , "--shadow" =: "0 1px 2px rgba(0, 0, 0, 0.4)"
      ]
    ]
  , CSS.selector_ "*"
    [ CSS.boxSizing "border-box"
    ]
  , CSS.selector_ "body"
    [ CSS.margin "0"
    , CSS.fontFamily "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"
    , CSS.backgroundColor (CSS.var "bg")
    , CSS.color (CSS.var "text")
    , CSS.lineHeight "1.5"
    ]
  , CSS.selector_ ".page"
    [ CSS.maxWidth "760px"
    , CSS.margin "0 auto"
    , CSS.padding "3rem 1rem"
    , CSS.display "flex"
    , CSS.flexDirection "column"
    , CSS.gap "1.5rem"
    ]
  , CSS.selector_ ".header h1"
    [ CSS.margin "0"
    , CSS.fontSize "2rem"
    , CSS.letterSpacing "-0.02em"
    ]
  , CSS.selector_ ".subtitle"
    [ CSS.margin "0.25rem 0 0"
    , CSS.color (CSS.var "muted")
    ]
  , CSS.selector_ "code"
    [ CSS.fontFamily "ui-monospace, SFMono-Regular, Menlo, monospace"
    , CSS.fontSize "0.9em"
    , CSS.padding "0.1em 0.35em"
    , CSS.borderRadius "4px"
    , CSS.backgroundColor (CSS.var "code-bg")
    ]
  , CSS.selector_ ".dropzone"
    [ CSS.position "relative"
    , CSS.display "flex"
    , CSS.flexDirection "column"
    , CSS.alignItems "center"
    , CSS.gap "0.25rem"
    , CSS.padding "2.5rem 1rem"
    , CSS.border "2px dashed var(--border)"
    , CSS.borderRadius "12px"
    , CSS.backgroundColor (CSS.var "surface")
    , CSS.textAlign "center"
    , CSS.transition "border-color 0.15s ease, background-color 0.15s ease"
    ]
  , CSS.selector_ ".dropzone:hover, .dropzone.dragging"
    [ CSS.borderColor (CSS.var "accent")
    , CSS.backgroundColor (CSS.var "accent-soft")
    ]
  , CSS.selector_ ".dropzone:focus-within"
    [ CSS.outline "2px solid var(--accent)"
    , "outline-offset" =: "2px"
    ]
  , CSS.selector_ ".file-input"
    [ CSS.position "absolute"
    , "inset" =: "0"
    , CSS.width "100%"
    , CSS.height "100%"
    , CSS.opacity 0
    , CSS.cursor "pointer"
    ]
  , CSS.selector_ ".dropzone-icon"
    [ CSS.fontSize "2rem"
    ]
  , CSS.selector_ ".dropzone-title"
    [ CSS.margin "0"
    , CSS.fontWeight "600"
    ]
  , CSS.selector_ ".browse"
    [ CSS.color (CSS.var "accent")
    , "text-decoration" =: "underline"
    ]
  , CSS.selector_ ".dropzone-hint"
    [ CSS.margin "0"
    , CSS.fontSize "0.875rem"
    , CSS.color (CSS.var "muted")
    ]
  , CSS.selector_ ".notice"
    [ CSS.padding "0.75rem 1rem"
    , CSS.borderRadius "8px"
    , CSS.backgroundColor (CSS.var "accent-soft")
    ]
  , CSS.selector_ ".notice.error"
    [ CSS.color (CSS.var "danger")
    , CSS.backgroundColor (CSS.var "danger-soft")
    ]
  , CSS.selector_ ".card"
    [ CSS.backgroundColor (CSS.var "surface")
    , CSS.border "1px solid var(--border)"
    , CSS.borderRadius "12px"
    , CSS.boxShadow "var(--shadow)"
    , CSS.overflow "hidden"
    ]
  , CSS.selector_ ".card-header"
    [ CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "space-between"
    , CSS.gap "1rem"
    , CSS.padding "1rem 1.25rem"
    , "border-bottom" =: "1px solid var(--border)"
    ]
  , CSS.selector_ ".file-meta"
    [ "min-width" =: "0"
    ]
  , CSS.selector_ ".file-name"
    [ CSS.margin "0"
    , CSS.fontSize "1rem"
    , CSS.overflow "hidden"
    , "text-overflow" =: "ellipsis"
    , CSS.whiteSpace "nowrap"
    ]
  , CSS.selector_ ".file-details"
    [ CSS.margin "0"
    , CSS.fontSize "0.875rem"
    , CSS.color (CSS.var "muted")
    ]
  , CSS.selector_ ".clear"
    [ "flex-shrink" =: "0"
    , CSS.padding "0.4rem 0.9rem"
    , CSS.border "1px solid var(--border)"
    , CSS.borderRadius "8px"
    , CSS.backgroundColor CSS.transparent
    , CSS.color (CSS.var "text")
    , "font" =: "inherit"
    , CSS.cursor "pointer"
    ]
  , CSS.selector_ ".clear:hover"
    [ CSS.borderColor (CSS.var "accent")
    , CSS.color (CSS.var "accent")
    ]
  , CSS.selector_ ".content"
    [ CSS.margin "0"
    , CSS.padding "1rem 1.25rem"
    , CSS.maxHeight "60vh"
    , CSS.overflow "auto"
    , CSS.fontFamily "ui-monospace, SFMono-Regular, Menlo, monospace"
    , CSS.fontSize "0.85rem"
    , CSS.lineHeight "1.6"
    , CSS.backgroundColor (CSS.var "code-bg")
    , "tab-size" =: "2"
    ]
  , CSS.selector_ ".empty"
    [ CSS.margin "0"
    , CSS.padding "1rem 1.25rem"
    , CSS.color (CSS.var "muted")
    ]
  , CSS.selector_ ".footer"
    [ CSS.fontSize "0.875rem"
    , CSS.textAlign "center"
    ]
  , CSS.selector_ ".footer a"
    [ CSS.color (CSS.var "muted")
    ]
  , CSS.media_ (CSS.prefersReducedMotion_ "reduce")
    [ CSS.rule_ ".dropzone"
      [ CSS.transition "none"
      ]
    ]
  ]
----------------------------------------------------------------------------
