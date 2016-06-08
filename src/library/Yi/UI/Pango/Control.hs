{-# LANGUAGE RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses
           , DeriveDataTypeable, OverloadedStrings, PatternSynonyms
           , GeneralizedNewtypeDeriving, FlexibleContexts #-}

-- this module isn't finished, and there's heaps of warnings.
{-# OPTIONS_GHC -w #-}

-- |
-- Module      :  Yi.UI.Pango.Control
-- License     :  GPL

module Yi.UI.Pango.Control (
    Control(..)
,   ControlM(..)
,   Buffer(..)
,   View(..)
,   Iter(..)
,   startControl
,   runControl
,   controlIO
,   liftYi
,   getControl
,   newBuffer
,   newView
,   getBuffer
,   setBufferMode
,   withCurrentBuffer
,   setText
,   getText
,   keyTable
,   newColor
,   newForeground
,   newBackground
,   yiForeground
,   yiBackground
,   yiColor
,   pangoScale
,   fromPango
,   toPango
) where

import Data.Text (unpack, pack, Text)
import qualified Data.Text as T
import Prelude hiding (concatMap, concat, foldl, elem, mapM_)
import Control.Exception (catch)
import Control.Monad        hiding (mapM_, forM_)
import Control.Monad.Reader hiding (mapM_, forM_)
import Control.Applicative
import Control.Lens hiding (views, Action)
import Data.Foldable
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Monoid
import Data.IORef
import Data.List (nub, filter, drop, zip, take, length)
import Data.Prototype
import Yi.Rope (toText, splitAtLine, YiString)
import qualified Yi.Rope as R
import qualified Data.Map as Map
import Yi.Core (startEditor, focusAllSyntax)
import Yi.Buffer
import Yi.Config
import Yi.Tab
import Yi.Window as Yi
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Monad
import Yi.Style
import Yi.UI.Utils
import Yi.Utils
import Yi.Debug
import Control.Monad.Reader (ask, asks, MonadReader(..))
import Control.Monad.State (ap, get, put, modify)
import Control.Monad.Base
import Control.Concurrent (newMVar, modifyMVar, MVar, newEmptyMVar, putMVar,
                           readMVar, isEmptyMVar)
import Data.Typeable
import qualified Data.List.PointedList as PL (insertRight, withFocus,
                                              PointedList(..), singleton)
import Yi.Regex
import Yi.String (showT)
import System.FilePath
import qualified Yi.UI.Common as Common
import Data.GI.Base
       (withManagedPtr, gflagsToWord, gerrorMessage, GError)
import GI.Gtk
       (noAdjustment, Clipboard, clipboardSetText, clipboardRequestText,
        clipboardGetForDisplay, widgetGetDisplay,
        onWidgetMotionNotifyEvent, onWidgetDraw, onWidgetScrollEvent,
        onWidgetButtonReleaseEvent, widgetGrabFocus,
        onWidgetButtonPressEvent, onWidgetKeyPressEvent,
        scrolledWindowSetPolicy, scrolledWindowAddWithViewport,
        scrolledWindowNew, widgetCreatePangoContext, widgetModifyBg,
        drawingAreaNew, ScrolledWindow, DrawingArea, widgetSetSizeRequest,
        widgetQueueDraw, mainQuit)
import qualified GI.Gtk as Gtk
       (setWidgetCanFocus, widgetAddEvents, widgetGetAllocatedHeight,
        widgetGetAllocatedWidth)
import GI.Pango
       (attrListInsert, attrListNew, AttrList, attrWeightNew,
        attrUnderlineNew, attrStyleNew, attrBackgroundNew,
        attrForegroundNew, attributeEndIndex, attributeStartIndex,
        attrClassType, AttrClass(..), attributeKlass, Attribute(..), Color,
        layoutIndexToPos, layoutGetCursorPos, layoutSetAttributes,
        layoutNew, contextGetMetrics, contextGetLanguage,
        layoutSetFontDescription, layoutXyToIndex, Layout,
        fontMetricsGetDescent, fontMetricsGetApproximateCharWidth,
        fontMetricsGetAscent, FontDescription, FontMetrics, Language,
        layoutSetText, layoutGetText, layoutGetPixelExtents,
        layoutSetWidth, layoutGetWidth, layoutGetFontDescription)
import GI.Gdk
       (keyvalToUnicode, keyvalName, EventKey, atomIntern, pattern BUTTON_MIDDLE,
        eventMotionReadY, eventMotionReadX, pattern BUTTON_PRIMARY, EventType,
        colorBlue, colorGreen, colorRed, eventScrollReadDirection,
        eventButtonReadButton, eventButtonReadType, eventButtonReadY,
        eventButtonReadX)
import Yi.UI.Pango.Rectangle (rectangleReadWidth, rectangleReadX, Rectangle)
import GI.Gtk.Enums (PolicyType(..), StateType(..))
import Data.GI.Base.Signals (SignalHandlerId)
import GI.Gdk.Flags (EventMask(..))
import qualified Data.GI.Base as Gtk (on, set)
import Data.GI.Base.Attributes (AttrOp(..))
import GI.Pango.Enums
       (AttrType(..), Weight(..), Underline(..), Style(..))
import GI.PangoCairo (showLayout)
import GI.Cairo (Context(..))
import qualified GI.Pango as Pango
       (rectangleReadHeight, rectangleReadWidth, rectangleReadY,
        rectangleReadX, Rectangle)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Graphics.Rendering.Cairo (setLineWidth)
import qualified Graphics.Rendering.Cairo as Cairo
       (setLineWidth, setSourceRGB, stroke, rectangle, lineTo, moveTo)
import qualified GI.Gdk as Gtk
       (ModifierType, eventKeyReadState, eventKeyReadKeyval,
        ScrollDirection, Color(..))
import qualified Graphics.Rendering.Cairo.Internal as Cairo
       (Render(..))
import Data.Word (Word32, Word16)
import Data.GI.Base.Constructible (Constructible(..))
import GI.Gdk.Enums (EventType(..))
import GI.GObject (signalHandlerDisconnect)
import qualified GI.Gdk.Enums as Gtk (ScrollDirection(..))
import qualified GI.Gdk.Flags as Gtk (ModifierType(..))
import Data.Int (Int32)
import Graphics.Rendering.Cairo.Internal (Render(..))
import Foreign.Ptr (castPtr)

data Control = Control
    { controlYi :: Yi
    , tabCache  :: IORef [TabInfo]
    , views     :: IORef (Map.Map WindowRef View)
    }
--    { config  :: Config
--    , editor  :: Editor
--    , input   :: Event -> IO ()
--    , output  :: Action -> IO ()
--    }

data TabInfo = TabInfo
    { coreTab     :: Tab
--    , page        :: VBox
    }

instance Show TabInfo where
    show t = show (coreTab t)

--type ControlM = YiM
newtype ControlM a = ControlM { runControl'' :: ReaderT Control IO a }
    deriving (Monad, MonadBase IO, MonadReader Control, Typeable,
              Functor, Applicative)

-- Helper functions to avoid issues with mismatching monad libraries
controlIO :: IO a -> ControlM a
controlIO = liftBase

getControl :: ControlM Control
getControl = ask

liftYi :: YiM a -> ControlM a
liftYi m = do
    yi <- asks controlYi
    liftBase $ runReaderT (runYiM m) yi

--instance MonadState Editor ControlM where
--    get = readRef =<< editor <$> ask
--    put v = flip modifyRef (const v) =<< editor <$> ask

--instance MonadEditor ControlM where
--    askCfg = config <$> ask
--    withEditor f = do
--      r <- asks editor
--      cfg <- asks config
--      liftBase $ controlUnsafeWithEditor cfg r f

startControl :: Config -> ControlM () -> IO ()
startControl config main = startEditor (config { startFrontEnd = start main } ) Nothing

runControl' :: ControlM a -> MVar Control -> IO (Maybe a)
runControl' m yiMVar = do
    empty <- isEmptyMVar yiMVar
    if empty
        then return Nothing
        else do
            yi <- readMVar yiMVar
            result <- runControl m yi
            return $ Just result

-- runControl :: ControlM a -> Yi -> IO a
-- runControl m yi = runReaderT (runYiM m) yi

runControl :: ControlM a -> Control -> IO a
runControl f = runReaderT (runControl'' f)

-- runControlEditor f yiMVar = yiMVar

runAction :: Action -> ControlM ()
runAction action = do
    out <- liftYi $ asks yiOutput
    liftBase $ out MustRefresh [action]

-- | Test 2
mkUI :: IO () -> MVar Control -> Common.UI Editor
mkUI main yiMVar = Common.dummyUI
    { Common.main          = main
    , Common.end           = \_ -> void $ runControl' end yiMVar
    , Common.suspend       = void $ runControl' suspend yiMVar
    , Common.refresh       = \e -> void $ runControl' (refresh e) yiMVar
    , Common.layout        = \e -> fromMaybe e <$> runControl' (doLayout e) yiMVar
    , Common.reloadProject = \f -> void $ runControl' (reloadProject f) yiMVar
    }

start :: ControlM () -> UIBoot
start main cfg ch outCh ed =
  catch (startNoMsg main cfg ch outCh ed) (\(e :: GError) ->
                                            fail . unpack =<< gerrorMessage e)

makeControl :: MVar Control -> YiM ()
makeControl controlMVar = do
    controlYi <- ask
    tabCache  <- liftBase $ newIORef []
    views  <- liftBase $ newIORef Map.empty
    liftBase $ putMVar controlMVar Control{..}

startNoMsg :: ControlM () -> UIBoot
startNoMsg main config input output ed = do
    control <- newEmptyMVar
    let wrappedMain = do
        output [makeAction $ makeControl control]
        void (runControl' main control)
    return (mkUI wrappedMain control)

end :: ControlM ()
end = do
    liftBase $ putStrLn "Yi Control End"
    liftBase mainQuit

suspend :: ControlM ()
suspend = do
    liftBase $ putStrLn "Yi Control Suspend"
    return ()

{-# ANN refresh ("HLint: ignore Redundant do" :: String) #-}
refresh :: Editor -> ControlM ()
refresh e = do
    --contextId <- statusbarGetContextId (uiStatusbar ui) "global"
    --statusbarPop  (uiStatusbar ui) contextId
    --statusbarPush (uiStatusbar ui) contextId $ intercalate "  " $ statusLine e

    updateCache e -- The cursor may have changed since doLayout
    viewsRef <- asks views
    vs <- liftBase $ readIORef viewsRef
    forM_ (Map.elems vs) $ \v -> do
        let b = findBufferWith (viewFBufRef v) e
        -- when (not $ null $ b ^. pendingUpdatesA) $
        do
            -- sig <- readIORef (renderer w)
            -- signalDisconnect sig
            -- writeRef (renderer w)
            -- =<< (textview w `onExpose` render e ui b (wkey (coreWin w)))
            liftBase $ widgetQueueDraw (drawArea v)

doLayout :: Editor -> ControlM Editor
doLayout e = do
    liftBase $ putStrLn "Yi Control Do Layout"
    updateCache e
    cacheRef <- asks tabCache
    tabs <- liftBase $ readIORef cacheRef
    dims <- concat <$> mapM (getDimensionsInTab e) tabs
    let e' = (tabsA %~ fmap (mapWindows updateWin)) e
        updateWin w = case find (\(ref,_,_,_) -> (wkey w == ref)) dims of
                          Nothing -> w
                          Just (_, wi, h,rgn) -> w { width = wi
                                                   , height = h
                                                   , winRegion = rgn }
    -- Don't leak references to old Windows
    let forceWin x w = height w `seq` winRegion w `seq` x
    return $ (foldl . tabFoldl) forceWin e' (e' ^. tabsA)

pangoScale :: Num a => a
pangoScale = 1024

fromPango :: Int32 -> Double
fromPango = (/pangoScale) . fromIntegral

toPango :: Double -> Int32
toPango = round . (*pangoScale)

-- | Width, Height
getDimensionsInTab :: Editor -> TabInfo -> ControlM [(WindowRef,Int,Int,Region)]
getDimensionsInTab e tab = do
  viewsRef <- asks views
  vs <- liftBase $ readIORef viewsRef
  foldlM (\a w ->
        case Map.lookup (wkey w) vs of
            Just v -> do
                wi <- liftBase $ Gtk.widgetGetAllocatedWidth $ drawArea v
                h <- liftBase $ Gtk.widgetGetAllocatedHeight $ drawArea v
                ascent    <- liftBase $ fontMetricsGetAscent (metrics v)
                descent   <- liftBase $ fontMetricsGetDescent (metrics v)
                charWidth <- liftBase $ fontMetricsGetApproximateCharWidth (metrics v)
                let lineHeight = ascent + descent
                    b0 = findBufferWith (viewFBufRef v) e
                rgn <- shownRegion e v b0
                let ret= (windowRef v, fromIntegral ((wi * pangoScale) `div` charWidth),
                          fromIntegral ((h * pangoScale) `div` lineHeight), rgn)
                return $ a <> [ret]
            Nothing -> return a)
      [] (coreTab tab ^. tabWindowsA)

shownRegion :: Editor -> View -> FBuffer -> ControlM Region
shownRegion e v b = do
   liftBase . print $ "shownRegion"
   (tos, _, bos) <- updatePango e v b (layout v)
   return $ mkRegion tos bos

updatePango :: Editor -> View -> FBuffer -> Layout
            -> ControlM (Point, Point, Point)
updatePango e v b layout = do
  width'  <- liftBase $ Gtk.widgetGetAllocatedWidth  $ drawArea v
  height' <- liftBase $ Gtk.widgetGetAllocatedHeight $ drawArea v

  liftBase . print $ "updatePango " ++ show (width', height')
  font <- liftBase $ layoutGetFontDescription layout

  --oldFont <- layoutGetFontDescription layout
  --oldFontStr <- maybe (return Nothing)
  --              (fmap Just . fontDescriptionToString) oldFont
  --newFontStr <- Just <$> fontDescriptionToString font
  --when (oldFontStr /= newFontStr)
  --  (layoutSetFontDescription layout (Just font))

  ascent  <- liftBase $ fontMetricsGetAscent  (metrics v)
  descent <- liftBase $ fontMetricsGetDescent (metrics v)
  let win                 = findWindowWith (windowRef v) e
      [width'', height''] = map (*pangoScale) [width', height']
      lineHeight          = ascent + descent
      winh                = max 1 $ fromIntegral (height'' `div` lineHeight)

      (tos, point, text)  = askBuffer win b $ do
                              from <- (use . markPointA) =<< fromMark <$> askMarks
                              rope <- streamB Forward from
                              p    <- pointB
                              let content = fst $ splitAtLine winh rope
                              -- allow BOS offset to be just after the last line
                              let addNL = if R.countNewLines content == winh
                                          then id
                                          else (`R.snoc` '\n')
                              return (from, p, R.toText $ addNL content)

  config   <- liftYi askCfg
  if configLineWrap $ configUI config
    then do oldWidth <- liftBase $ layoutGetWidth layout
            when (oldWidth /= width'') $
              liftBase $ layoutSetWidth layout width''
    else liftBase $ do
      (r, _) <- layoutGetPixelExtents layout
      px <- Pango.rectangleReadX r
      pwidth <- Pango.rectangleReadWidth r
      widgetSetSizeRequest (drawArea v) (px+pwidth) (-1)

  -- optimize for cursor movement
  oldText <- liftBase $ layoutGetText layout
  when (oldText /= text) $ liftBase $ layoutSetText layout text (-1)

  (_, bosOffset, _) <- liftBase $ layoutXyToIndex layout width''
                       (fromIntegral winh * lineHeight - 1)
  return (tos, point, tos + fromIntegral bosOffset + 1)

updateCache :: Editor -> ControlM ()
updateCache e = do
    let tabs = e ^. tabsA
    cacheRef <- asks tabCache
    cache <- liftBase $ readIORef cacheRef
    cache' <- syncTabs e (toList $ PL.withFocus tabs) cache
    liftBase $ writeIORef cacheRef cache'

syncTabs :: Editor -> [(Tab, Bool)] -> [TabInfo] -> ControlM [TabInfo]
syncTabs e (tfocused@(t,focused):ts) (c:cs)
    | t == coreTab c =
        do when focused $ setTabFocus c
--           let vCache = views c
           (:) <$> syncTab e c t <*> syncTabs e ts cs
    | t `elem` map coreTab cs =
        do removeTab c
           syncTabs e (tfocused:ts) cs
    | otherwise =
        do c' <- insertTabBefore e t c
           when focused $ setTabFocus c'
           return (c':) `ap` syncTabs e ts (c:cs)
syncTabs e ts [] = mapM (\(t,focused) -> do
        c' <- insertTab e t
        when focused $ setTabFocus c'
        return c') ts
syncTabs _ [] cs = mapM_ removeTab cs >> return []

syncTab :: Editor -> TabInfo -> Tab -> ControlM TabInfo
syncTab e tab ws =
  -- TODO Maybe do something here
  return tab

setTabFocus :: TabInfo -> ControlM ()
setTabFocus t =
  -- TODO this needs to set the tab focus with callback
  -- but only if the tab focus has changed
  return ()

askBuffer :: Yi.Window -> FBuffer -> BufferM a -> a
askBuffer w b f = fst $ runBuffer w b f

setWindowFocus :: Editor -> TabInfo -> View -> ControlM ()
setWindowFocus e t v = do
  let bufferName = shortIdentString (length $ commonNamePrefix e) $
                   findBufferWith (viewFBufRef v) e
      window = findWindowWith (windowRef v) e
      ml = askBuffer window (findBufferWith (viewFBufRef v) e) $
           getModeLine (T.pack <$> commonNamePrefix e)

-- TODO
--  update (textview w) widgetIsFocus True
--  update (modeline w) labelText ml
--  update (uiWindow ui) windowTitle $ bufferName <> " - Yi"
--  update (uiNotebook ui) (notebookChildTabLabel (page t))
--    (tabAbbrevTitle bufferName)
  return ()

removeTab :: TabInfo -> ControlM ()
removeTab t =
  -- TODO this needs to close the views in the tab with callback
  return ()

removeView :: TabInfo -> View -> ControlM ()
removeView tab view =
  -- TODO this needs to close the view with callback
  return ()

-- | Make a new tab.
newTab :: Editor -> Tab -> ControlM TabInfo
newTab e ws = do
    let t' = TabInfo { coreTab = ws }
--    cache <- syncWindows e t' (toList $ PL.withFocus ws) []
    return t' -- { views = cache }

{-# ANN insertTabBefore ("HLint: ignore Redundant do" :: String) #-}
insertTabBefore :: Editor -> Tab -> TabInfo -> ControlM TabInfo
insertTabBefore e ws c = do
    -- Just p <- notebookPageNum (uiNotebook ui) (page c)
    -- vb <- vBoxNew False 1
    -- notebookInsertPage (uiNotebook ui) vb "" p
    -- widgetShowAll $ vb
    newTab e ws

{-# ANN insertTab ("HLint: ignore Redundant do" :: String) #-}
insertTab :: Editor -> Tab -> ControlM TabInfo
insertTab e ws = do
    -- vb <- vBoxNew False 1
    -- notebookAppendPage (uiNotebook ui) vb ""
    -- widgetShowAll $ vb
    newTab e ws

{-
insertWindowBefore :: Editor -> TabInfo -> Yi.Window -> WinInfo -> IO WinInfo
insertWindowBefore e ui tab w _c = insertWindow e ui tab w

insertWindowAtEnd :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindowAtEnd e ui tab w = insertWindow e ui tab w

insertWindow :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindow e ui tab win = do
  let buf = findBufferWith (bufkey win) e
  liftBase $ do w <- newWindow e ui win buf

              set (page tab) $
                [ containerChild := widget w
                , boxChildPacking (widget w) :=
                    if isMini (coreWin w)
                        then PackNatural
                        else PackGrow
                ]

              let ref = (wkey . coreWin) w
              textview w `onButtonRelease` handleClick ui ref
              textview w `onButtonPress` handleClick ui ref
              textview w `onScroll` handleScroll ui ref
              textview w `onConfigure` handleConfigure ui ref
              widgetShowAll (widget w)

              return w
-}

reloadProject :: FilePath -> ControlM ()
reloadProject _ = return ()

controlUnsafeWithEditor :: Config -> MVar Editor -> EditorM a -> IO a
controlUnsafeWithEditor cfg r f = modifyMVar r $ \e -> do
  let (e',a) = runEditor cfg f e
  -- Make sure that the result of runEditor is evaluated before
  -- replacing the editor state. Otherwise, we might replace e
  -- with an exception-producing thunk, which makes it impossible
  -- to look at or update the editor state.
  -- Maybe this could also be fixed by -fno-state-hack flag?
  -- TODO: can we simplify this?
  e' `seq` a `seq` return (e', a)

data Buffer = Buffer
    { fBufRef     :: BufferRef
    }

data View = View
    { viewFBufRef :: BufferRef
    , windowRef   :: WindowRef
    , drawArea    :: DrawingArea
    , layout      :: Layout
    , language    :: Language
    , metrics     :: FontMetrics
    , scrollWin   :: ScrolledWindow
    , shownTos    :: IORef Point
    , winMotionSignal :: IORef (Maybe SignalHandlerId)
    }

data Iter = Iter
    { iterFBufRef :: BufferRef
    , point       :: Point
    }

newBuffer :: BufferId -> R.YiString -> ControlM Buffer
newBuffer id text = do
    fBufRef <- liftYi . withEditor . newBufferE id $ text
    return Buffer{..}

newView :: Buffer -> FontDescription -> ControlM View
newView buffer font = do
    control  <- ask
    config   <- liftYi askCfg
    let viewFBufRef = fBufRef buffer
    newWindow <-
      fmap (\w -> w { height=50
                    , winRegion = mkRegion (Point 0) (Point 2000)
                    }) $ liftYi $ withEditor $ newWindowE False viewFBufRef
    let windowRef = wkey newWindow
    liftYi $ withEditor $ do
        windowsA %= PL.insertRight newWindow
        e <- get
        put $ focusAllSyntax e
    drawArea <- liftBase drawingAreaNew
    liftBase . widgetModifyBg drawArea StateTypeNormal . Just =<< liftBase (newBackground
      . Yi.Style.background . baseAttributes . configStyle $ configUI config)
    context  <- liftBase $ widgetCreatePangoContext drawArea
    layout   <- liftBase $ layoutNew context
    liftBase $ layoutSetFontDescription layout (Just font)
    language <- liftBase $ contextGetLanguage context
    metrics  <- liftBase $ contextGetMetrics context (Just font) (Just language)
    liftBase $ layoutSetText layout "" (-1)

    scrollWin <- liftBase $ scrolledWindowNew noAdjustment noAdjustment
    liftBase $ do
        scrolledWindowAddWithViewport scrollWin drawArea
        scrolledWindowSetPolicy scrollWin PolicyTypeAutomatic PolicyTypeNever

    initialTos <-
      liftYi . withEditor . withGivenBufferAndWindow newWindow viewFBufRef $
        (use . markPointA) =<< fromMark <$> askMarks
    shownTos <- liftBase $ newIORef initialTos
    winMotionSignal <- liftBase $ newIORef Nothing

    let view = View {..}

    liftBase $ Gtk.widgetAddEvents drawArea (gflagsToWord [EventMaskKeyPressMask])
    liftBase $ Gtk.setWidgetCanFocus drawArea True

    liftBase $ onWidgetKeyPressEvent drawArea $ \e -> do
        -- putStrLn $ "Yi Control Key Press = " <> show event
        liftBase $ runControl (runAction $ makeAction $ do
            focusWindowE windowRef
            switchToBufferE viewFBufRef) control
        result <- processKeyEvent (yiInput $ controlYi control) e
        liftBase $ widgetQueueDraw drawArea
        return result

    liftBase $ onWidgetButtonPressEvent drawArea $ \e -> do
        x <- eventButtonReadX e
        y <- eventButtonReadY e
        click <- eventButtonReadType e
        button <- eventButtonReadButton e
        liftBase $ do
            widgetGrabFocus drawArea
            runControl (handleClick view x y click button) control

    liftBase $ onWidgetButtonReleaseEvent drawArea $ \e -> do
        x <- eventButtonReadX e
        y <- eventButtonReadY e
        click <- eventButtonReadType e
        button <- eventButtonReadButton e
        liftBase $ runControl (handleClick view x y click button) control

    liftBase $ onWidgetScrollEvent drawArea $ \e -> do
        direction <- eventScrollReadDirection e
        liftBase $ runControl (handleScroll view direction) control

    liftBase $ onWidgetDraw drawArea $ \context -> do
        (text, allAttrs, debug, tos, rel, point, inserting) <- liftIO $ runControl (liftYi $ withEditor $ do
            window <- (findWindowWith windowRef) <$> get
            (%=) buffersA (fmap (clearSyntax . clearHighlight))
            let winh = height window
            let tos = max 0 (regionStart (winRegion window))
            let bos = regionEnd (winRegion window)
            let rel p = fromIntegral (p - tos)

            withGivenBufferAndWindow window viewFBufRef $ do
                -- tos       <- getMarkPointB =<< fromMark <$> askMarks
                rope      <- streamB Forward tos
                point     <- pointB
                inserting <- use insertingA

                modeNm <- gets (withMode0 modeName)

    --            let (tos, point, text, picture) = do runBu
    --                        from     <- getMarkPointB =<< fromMark <$> askMarks
    --                        rope     <- streamB Forward from
    --                        p        <- pointB
                let content = fst $ splitAtLine winh rope
                -- allow BOS offset to be just after the last line
                let addNL = if R.countNewLines content == winh
                              then id
                              else (`R.snoc` '\n')
                    sty = configStyle $ configUI config
                          -- attributesPictureAndSelB sty (currentRegex e)
                          --   (mkRegion tos bos)
                          -- return (from, p, addNL $ Rope.toString content,
                          --         picture)
                let text = R.toText $ addNL content

                picture <- attributesPictureAndSelB sty Nothing
                           (mkRegion tos bos)

                -- add color attributes.
                let picZip = zip picture $ drop 1 (fst <$> picture) <> [bos]
                    strokes = [ (start',s,end') | ((start', s), end') <- picZip
                                                , s /= emptyAttributes ]

                    rel p = fromIntegral (p - tos)
                    allAttrs :: MonadIO m => m AttrList
                    allAttrs = do
                      list <- attrListNew
                      forM_ strokes $ \(p1, Attributes fg bg _rv bd itlc udrl, p2) -> do
                        let atr :: MonadIO m => m Attribute -> m ()
                            atr newAttr = do
                                a <- newAttr
                                Gtk.set a
                                    [ attributeStartIndex := rel p1
                                    , attributeEndIndex := rel p2
                                    ]
                                attrListInsert list a
                        let if' p x y = if p then x else y
                        mapM_ atr
                             [ yiForeground attrForegroundNew fg
                             , yiBackground attrBackgroundNew bg
                             , attrStyleNew $ if' itlc StyleItalic StyleNormal
                             , attrUnderlineNew $
                                 if' udrl UnderlineSingle UnderlineNone
                             , attrWeightNew $ if' bd WeightBold WeightNormal
                             ]
                      return list

                return (text, allAttrs, (picture, strokes, modeNm,
                                         window, tos, bos, winh),
                        tos, rel, point, inserting)) control

        liftIO $ do
            -- putStrLn $ "Setting Layout Attributes " <> show debug
            layoutSetAttributes layout . Just =<< allAttrs
            -- putStrLn "Done Stting Layout Attributes"
            -- dw      <- widgetGetDrawWindow drawArea
            -- gc      <- gcNew dw
            oldText <- layoutGetText layout
            when (text /= oldText) $ layoutSetText layout text (-1)
        showLayout context layout
        liftIO $ writeIORef shownTos tos

        -- paint the cursor
        (curRect, _) <- layoutGetCursorPos layout (rel point)
        curx <- Pango.rectangleReadX curRect
        cury <- Pango.rectangleReadY curRect
        curw <- Pango.rectangleReadWidth curRect
        curh <- Pango.rectangleReadHeight curRect
        chRect <- liftIO $ layoutIndexToPos layout (rel point)
        chx <- Pango.rectangleReadX chRect
        chy <- Pango.rectangleReadY chRect
        chw <- Pango.rectangleReadWidth chRect
        chh <- Pango.rectangleReadHeight chRect

        -- gcSetValues gc (newGCValues { Gtk.foreground = newForeground $ Yi.Style.foreground $ baseAttributes $ configStyle $ configUI config })
        withManagedPtr context $ \cPtr -> (`runReaderT` Cairo (castPtr cPtr)) . runRender $ do
            sourceCol True $ Yi.Style.foreground $ baseAttributes $ configStyle $ configUI config
            Cairo.setLineWidth 2
            if inserting
                then do
                    Cairo.moveTo (fromPango curx) (fromPango cury)
                    Cairo.lineTo (fromPango curx + fromPango curw) (fromPango cury + fromPango curh)
                else Cairo.rectangle (fromPango chx) (fromPango chy) (if chw > 0 then fromPango chw else 8) (fromPango chh)
            Cairo.stroke

        return True

    liftBase $ widgetGrabFocus drawArea

    tabsRef <- asks tabCache
    ts <- liftBase $ readIORef tabsRef
    -- TODO: the Tab idkey should be assigned using
    -- Yi.Editor.newRef. But we can't modify that here, since our
    -- access to 'Yi' is readonly.
    liftBase $ writeIORef tabsRef (TabInfo (makeTab1 0 newWindow):ts)

    viewsRef <- asks views
    vs <- liftBase $ readIORef viewsRef
    liftBase $ writeIORef viewsRef $ Map.insert windowRef view vs

    liftBase . print $ "added window ref" ++ show windowRef

    return view
  where
    clearHighlight fb =
      -- if there were updates, then hide the selection.
      let h = view highlightSelectionA fb
          us = view pendingUpdatesA fb
      in highlightSelectionA .~ (h && null us) $ fb

{-# ANN setBufferMode ("HLint: ignore Redundant do" :: String) #-}
setBufferMode :: FilePath -> Buffer -> ControlM ()
setBufferMode f buffer = do
    let bufRef = fBufRef buffer
    -- adjust the mode
    tbl <- liftYi $ asks (modeTable . yiConfig)
    contents <- liftYi $ withGivenBuffer bufRef elemsB
    let header = R.toString $ R.take 1024 contents
        hmode = case header =~ ("\\-\\*\\- *([^ ]*) *\\-\\*\\-" :: String) of
            AllTextSubmatches [_,m] -> T.pack m
            _ -> ""
        Just mode = find (\(AnyMode m)-> modeName m == hmode) tbl <|>
                    find (\(AnyMode m)-> modeApplies m f contents) tbl <|>
                    Just (AnyMode emptyMode)
    case mode of
        AnyMode newMode -> do
            -- liftBase $ putStrLn $ show (f, modeName newMode)
            liftYi $ withEditor $ do
                withGivenBuffer bufRef $ do
                    setMode newMode
                    modify clearSyntax
                switchToBufferE bufRef
            -- withEditor focusAllSyntax

withBuffer :: Buffer -> BufferM a -> ControlM a
withBuffer Buffer{fBufRef = b} f = liftYi $ withGivenBuffer b f

getBuffer :: View -> Buffer
getBuffer view = Buffer {fBufRef = viewFBufRef view}

setText :: Buffer -> YiString -> ControlM ()
setText b text = withBuffer b $ do
    r <- regionOfB Document
    replaceRegionB r text

getText :: Buffer -> Iter -> Iter -> ControlM Text
getText b Iter{point = p1} Iter{point = p2} =
  fmap toText . withBuffer b . readRegionB $ mkRegion p1 p2

newColor :: MonadIO m => Word16 -> Word16 -> Word16 -> m Gtk.Color
newColor r g b = new Gtk.Color [colorRed := r, colorGreen := g, colorBlue := b]

newForeground, newBackground :: MonadIO m => Yi.Style.Color -> m Gtk.Color
newForeground = yiForeground newColor
newBackground = yiBackground newColor

yiForeground, yiBackground :: (Word16 -> Word16 -> Word16 -> a) -> Yi.Style.Color -> a
yiForeground = yiColor 0
yiBackground = yiColor maxBound

yiColor :: Word16 -> (Word16 -> Word16 -> Word16 -> a) -> Yi.Style.Color -> a
yiColor d f Default = f d d d
yiColor _ f (RGB x y z) = f (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

sourceCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Cairo.Render ()
sourceCol True  Default = Cairo.setSourceRGB 0 0 0
sourceCol False Default = Cairo.setSourceRGB 1 1 1
sourceCol _ (RGB r g b) = Cairo.setSourceRGB (fromIntegral r / 255)
                                             (fromIntegral g / 255)
                                             (fromIntegral b / 255)

handleClick :: View -> Double -> Double -> EventType -> Word32 -> ControlM Bool
handleClick view x y click button = do
  control  <- ask
  -- (_tabIdx,winIdx,w) <- getWinInfo ref <$> readIORef (tabCache ui)

  logPutStrLn $ "Click: " <> showT (x, y, click)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- io $ layoutXyToIndex (layout view) (toPango x) (toPango y)
  tos <- liftBase $ readIORef (shownTos view)
  let p1 = tos + fromIntegral layoutIndex

  let winRef = windowRef view

  -- maybe focus the window
  -- logPutStrLn $ "Clicked inside window: " <> show view

--  let focusWindow = do
      -- TODO: check that tabIdx is the focus?
--      (%=) windowsA (fromJust . PL.move winIdx)

  liftBase $ case (click, fromIntegral button) of
     (EventTypeButtonPress, BUTTON_PRIMARY) -> do
        cid <- onWidgetMotionNotifyEvent (drawArea view) $ \e -> do
            x <- eventMotionReadX e
            y <- eventMotionReadY e
            liftBase $ runControl (handleMove view p1 x y) control
        writeIORef (winMotionSignal view) $ Just cid

     _ -> do
       maybe (return ()) (signalHandlerDisconnect (drawArea view)) =<< readIORef (winMotionSignal view)
       writeIORef (winMotionSignal view) Nothing

  case (click, fromIntegral button) of
    (EventTypeButtonPress, BUTTON_PRIMARY) -> runAction . EditorA $ do
        -- b <- gets $ (bkey . findBufferWith (viewFBufRef view))
        -- focusWindow
        window <- findWindowWith winRef <$> get
        withGivenBufferAndWindow window (viewFBufRef view) $ do
            moveTo p1
            setVisibleSelection False
    -- (Gtk.SingleClick, _) -> runAction focusWindow
    (EventTypeButtonRelease, BUTTON_MIDDLE) -> do
        disp <- liftBase $ widgetGetDisplay (drawArea view)
        cb <- liftBase $ clipboardGetForDisplay disp =<< atomIntern "PRIMARY" False
        let cbHandler :: Clipboard -> Maybe Text -> IO ()
            cbHandler _ Nothing = return ()
            cbHandler _ (Just txt) = runControl (runAction . EditorA $ do
                window <- findWindowWith winRef <$> get
                withGivenBufferAndWindow window (viewFBufRef view) $ do
                    pointB >>= setSelectionMarkPointB
                    moveTo p1
                    insertN (R.fromText txt)) control
        liftBase $ clipboardRequestText cb cbHandler
    _ -> return ()

  liftBase $ widgetQueueDraw (drawArea view)
  return True

handleScroll :: View -> Gtk.ScrollDirection -> ControlM Bool
handleScroll view direction = do
  let editorAction =
        withCurrentBuffer $ vimScrollB $ case direction of
                        Gtk.ScrollDirectionUp   -> -1
                        Gtk.ScrollDirectionDown -> 1
                        _ -> 0 -- Left/right scrolling not supported

  runAction $ EditorA editorAction
  liftBase $ widgetQueueDraw (drawArea view)
  return True

handleMove :: View -> Point -> Double -> Double -> ControlM Bool
handleMove view p0 x y = do
  logPutStrLn $ "Motion: " <> showT (x, y)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- liftBase $ layoutXyToIndex (layout view) (toPango x) (toPango y)
  tos <- liftBase $ readIORef (shownTos view)
  let p1 = tos + fromIntegral layoutIndex


  let editorAction = do
        txt <- withCurrentBuffer $
           if p0 /= p1
            then Just <$> do
              m <- selMark <$> askMarks
              markPointA m .= p0
              moveTo p1
              setVisibleSelection True
              readRegionB =<< getSelectRegionB
            else return Nothing
        maybe (return ()) setRegE txt

  runAction $ makeAction editorAction
  -- drawWindowGetPointer (textview w) -- be ready for next message.

  -- Relies on uiActionCh being synchronous
  selection <- liftBase $ newIORef ""
  let yiAction = do
      txt <- withCurrentBuffer (readRegionB =<< getSelectRegionB)
             :: YiM R.YiString
      liftBase $ writeIORef selection txt
  runAction $ makeAction yiAction
  txt <- liftBase $ readIORef selection

  disp <- liftBase $ widgetGetDisplay (drawArea view)
  cb <- liftBase $ clipboardGetForDisplay disp =<< atomIntern "PRIMARY" False
  liftBase $ clipboardSetText cb (R.toText txt) (-1)

  liftBase $ widgetQueueDraw (drawArea view)
  return True

processKeyEvent :: ([Event] -> IO ()) -> EventKey -> IO Bool
processKeyEvent ch ev = do
  -- logPutStrLn $ "Gtk.Event: " <> show ev
  -- logPutStrLn $ "Event: " <> show (gtkToYiEvent ev)
  keyval <- Gtk.eventKeyReadKeyval ev
  name   <- keyvalName keyval
  mod    <- Gtk.eventKeyReadState ev
  char   <- toEnum . fromIntegral <$> keyvalToUnicode keyval
  case gtkToYiEvent name mod char of
    Nothing -> logPutStrLn $ "Event not translatable: " <> showT (name, mod, char)
    Just e -> liftBase $ ch [e]
  return True

gtkToYiEvent :: Maybe Text -> [Gtk.ModifierType] -> Char -> Maybe Event
gtkToYiEvent key evModifier char
    = fmap (\k -> Event k $ (nub $ (if isShift then filter (/= MShift) else id) $ concatMap modif evModifier)) key'
      where (key',isShift) =
                case (char, key) of
                  (c, _) | c >= ' ' -> (Just $ KASCII c, True)
                  (_, Just k)       -> (Map.lookup k keyTable, False)
                  _                 -> (Nothing, False)
            modif Gtk.ModifierTypeControlMask = [MCtrl]
            modif Gtk.ModifierTypeMetaMask    = [MMeta]
            modif Gtk.ModifierTypeShiftMask   = [MShift]
            modif _ = []

-- | Map GTK long names to Keys
keyTable :: Map.Map Text Key
keyTable = Map.fromList
    [("Down",       KDown)
    ,("Up",         KUp)
    ,("Left",       KLeft)
    ,("Right",      KRight)
    ,("Home",       KHome)
    ,("End",        KEnd)
    ,("BackSpace",  KBS)
    ,("Delete",     KDel)
    ,("Page_Up",    KPageUp)
    ,("Page_Down",  KPageDown)
    ,("Insert",     KIns)
    ,("Escape",     KEsc)
    ,("Return",     KEnter)
    ,("Tab",        KTab)
    ,("ISO_Left_Tab", KTab)
    ]
