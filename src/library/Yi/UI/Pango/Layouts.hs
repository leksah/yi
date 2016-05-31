{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.UI.Pango.Layouts
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides abstract controls which implement 'Yi.Layout.Layout's and
-- which manage the minibuffer.
--
-- The implementation strategy is to first construct the layout
-- managers @WeightedStack@ (implementing the 'Stack' constructor) and
-- @SlidingPair@ (implementing the 'Pair' constructor), and then
-- construct 'LayoutDisplay' as a tree of these, mirroring the
-- structure of 'Layout'.

module Yi.UI.Pango.Layouts (
  -- * Getting the underlying widget
  WidgetLike(..),
  -- * Window layout
  LayoutDisplay,
  layoutDisplayNew,
  layoutDisplaySet,
  layoutDisplayOnDividerMove,
  -- * Miniwindow layout
  MiniwindowDisplay,
  miniwindowDisplayNew,
  miniwindowDisplaySet,
  -- * Tabs
  SimpleNotebook,
  simpleNotebookNew,
  simpleNotebookSet,
  simpleNotebookOnSwitchPage,
  -- * Utils
  update,
 ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad hiding (mapM, forM)
import           Data.Foldable (toList)
import           Data.IORef
import qualified Data.List.PointedList as PL
import qualified Data.Text as T
import           Data.Traversable
import           Prelude hiding (mapM)
import           Yi.Layout(Orientation(..), RelativeSize, DividerPosition,
                           Layout(..), DividerRef)
import GI.Gtk.Objects.Widget
       (WidgetK, onWidgetSizeAllocate, toWidget, widgetShowAll,
        widgetSizeAllocate, widgetSizeRequest, Widget(..))
import GI.Gtk.Objects.Fixed (fixedNew, Fixed(..))
import Data.GI.Base
       (unsafeManagedPtrGetPtr, unsafeCastTo, GObject, castTo, set)
import GI.Gtk.Objects.Container
       (ContainerK, containerRemove, Container(..), toContainer,
        containerChild)
import Data.GI.Base.Attributes
       (AttrLabelProxy(..), get, AttrOp(..))
import qualified Data.GI.Base.Signals as Gtk (on)
import GI.Gtk.Structs.Requisition
       (requisitionHeight, requisitionWidth, requisitionReadHeight,
        requisitionReadWidth, Requisition(..))
import GI.Gdk.Objects.Screen
       (screenGetHeight, screenGetWidth, screenGetDefault)
import Yi.UI.Pango.Rectangle
       (rectangleReadHeight, rectangleReadWidth, rectangleReadY,
        rectangleReadX, rectangleHeight, rectangleWidth, rectangleY,
        rectangleX, Rectangle(..), newRectangle)
import GI.Gtk.Objects.Paned
       (getPanedPosition, panedPosition, panedPack2, panedPack1, toPaned,
        Paned(..))
import GI.Gtk.Objects.HPaned (hPanedNew)
import GI.Gtk.Objects.VPaned (vPanedNew)
import GI.Gtk.Objects.Bin (toBin, Bin(..))
import GI.Gtk.Objects.Alignment (alignmentNew)
import GI.Gtk.Objects.VBox (vBoxNew, VBox(..))
import GI.Gtk.Objects.Box (boxPackEnd)
import GI.Gtk.Objects.Notebook
       (setNotebookPage, getNotebookPage, notebookSetTabLabel,
        notebookGetTabLabel, onNotebookSwitchPage, notebookPage,
        notebookPageNum, notebookAppendPage, notebookRemovePage,
        notebookNew, Notebook(..))
import Data.GI.Base.Signals (on)
import Data.Int (Int32)
import Data.GI.Base.Constructible (Constructible(..))
import Data.GI.Base.BasicTypes (GObject(..), NullToNothing(..))
import GI.Gtk.Objects.Label
       (Label(..), setLabelLabel, getLabelLabel, labelNew)
import Data.Word (Word32)
import Foreign.ForeignPtr (ForeignPtr)
import Data.GI.Base.Overloading (ParentTypes)
import GI.GObject (Object)

class WidgetLike w where
  -- | Extracts the main widget. This is the widget to be added to the GUI.
  baseWidget :: w -> IO Widget

----------------------- The WeightedStack type
{- | A @WeightedStack@ is like a 'VBox' or 'HBox', except that we may
specify the ratios of the areas of the child widgets (so this
implements the 'Stack' constructor of 'Yi.Layout.Layout'.

Essentially, we implement this layout manager from scratch, by
implementing the 'sizeRequest' and 'sizeAllocate' signals by hand (see
the 'Container' documentation for details, and
http://www.ibm.com/developerworks/linux/library/l-widget-pygtk/ for an
example in Python). Ideally, we would directly subclass the abstract
class 'Container', but Gtk2hs doesn't directly support this. Instead,
we start off with the concrete class 'Fixed', and just override its
layout behaviour.
-}

newtype WeightedStack = WeightedStack (ForeignPtr WeightedStack)

type instance ParentTypes WeightedStack = WeightedStackParentTypes
type WeightedStackParentTypes = '[Fixed, Container, Widget, Object]

instance GObject WeightedStack where
    gobjectIsInitiallyUnowned _ = False
    gobjectType _ = gobjectType (undefined :: Fixed)

type StackDescr = [(Widget, RelativeSize)]

weightedStackNew :: Orientation -> StackDescr -> IO WeightedStack
weightedStackNew o s = do
  when (any ((<= 0) . snd) s) $ error
    "Yi.UI.Pango.WeightedStack.WeightedStack: all weights must be positive"
  l <- fixedNew
  set l (fmap ((containerChild :=) . fst) s)
  void $ onWidgetSizeAllocate l (relayout o s)
  unsafeCastTo WeightedStack l

-- | Requests the smallest size so that each widget gets its requested size
doSizeRequest :: Orientation -> StackDescr -> IO Requisition
doSizeRequest o s =
  let
    (requestAlong, requestAcross) =
      case o of
        Horizontal ->
          (\r -> fromIntegral <$> requisitionReadWidth r,
           requisitionReadHeight)
        Vertical ->
          (\r -> fromIntegral <$> requisitionReadHeight r,
           requisitionReadWidth)

    totalWeight = sum . fmap snd $ s
    reqsize :: (Requisition, RelativeSize) -> IO RelativeSize
    reqsize (request, relSize) = (/ relSize) <$> requestAlong request
    sizeAlong :: [(Requisition, RelativeSize)] -> IO RelativeSize
    sizeAlong widgetRequests =
      (totalWeight *) . maximum <$> mapM reqsize widgetRequests
    sizeAcross :: [(Requisition, RelativeSize)] -> IO Int32
    sizeAcross widgetRequests =
      maximum <$> mapM (requestAcross . fst) widgetRequests
    mkRequisition :: [(Requisition, RelativeSize)] -> IO Requisition
    mkRequisition wr = do
      along <- round <$> sizeAlong wr
      across <- sizeAcross wr
      case o of
        Horizontal -> new Requisition [requisitionWidth := along,  requisitionHeight := across]
        Vertical   -> new Requisition [requisitionWidth := across, requisitionHeight := along]
    swreq (w, relSize) = (,relSize) <$> widgetSizeRequest w
  in
   boundRequisition =<< mkRequisition =<< mapM swreq s


-- | Bounds the given requisition to not exceed screen dimensions
boundRequisition :: Requisition -> IO Requisition
boundRequisition r =
  do
    mscr <- screenGetDefault
    case mscr of
      Just scr -> do
        w <- requisitionReadWidth r
        h <- requisitionReadHeight r
        new Requisition [ requisitionWidth  :=> min w <$> screenGetWidth scr
                        , requisitionHeight :=> min h <$> screenGetHeight scr ]
      Nothing -> return r

-- | Position the children appropriately for the given width and height
relayout :: Orientation -> StackDescr -> Rectangle -> IO ()
relayout o s r = do
  x <- rectangleReadX r
  y <- rectangleReadY r
  width <- rectangleReadWidth r
  height <- rectangleReadHeight r
  let
    totalWeight = sum . fmap snd $ s
    totalSpace = fromIntegral $
      case o of
        Horizontal -> width
        Vertical -> height
    wtMult = totalSpace / totalWeight
    calcPosition pos (widget, wt) = (pos + wt * wtMult,
                                     (pos, wt * wtMult, widget))
    widgetToRectangle (round -> pos, round -> size, widget) =
      (, widget) <$>
        case o of
          Horizontal -> newRectangle [rectangleX := pos, rectangleY := y, rectangleWidth := size, rectangleHeight := height]
          Vertical -> newRectangle [rectangleX := x, rectangleY := pos, rectangleWidth := width, rectangleHeight := size]
    startPosition = fromIntegral $
      case o of
        Horizontal -> x
        Vertical -> y
    widgetPositions =
      mapM widgetToRectangle (snd (mapAccumL calcPosition startPosition s))
  widgetPositions >>= mapM_ (\(rect, widget) -> widgetSizeAllocate widget rect)

------------------------------------------------------- SlidingPair

{-|
'SlidingPair' implements the 'Pair' constructor.

Most of what is needed is already implemented by the 'HPaned' and
'VPaned' classes. The main feature added by 'SlidingPair' is that the
divider position, *as a fraction of the available space*, remains
constant even when resizing.
-}

newtype SlidingPair = SlidingPair (ForeignPtr SlidingPair)

type instance ParentTypes SlidingPair = SlidingPairParentTypes
type SlidingPairParentTypes = '[Paned, Container, Widget, Object]

instance GObject SlidingPair where
    gobjectIsInitiallyUnowned _ = False
    gobjectType _ = gobjectType (undefined :: Paned)

slidingPairNew :: (WidgetK w1, WidgetK w2) => Orientation -> w1 -> w2
               -> DividerPosition
               -> (DividerPosition -> IO ())
               -> IO SlidingPair
slidingPairNew o w1 w2 pos handleNewPos = do
  p <-
    case o of
      Horizontal -> hPanedNew >>= toPaned
      Vertical -> vPanedNew >>= toPaned
  panedPack1 p w1 True True
  panedPack2 p w2 True True

{- We want to catch the sizeAllocate signal. If this event is
called, two things could have happened: the size could have changed;
or the slider could have moved.  We want to correct the slider
position, but only if the size has changed. Furthermore, if the size
only changes in the direction /orthogonal/ to the slider, then there
is also no need to correct the slider position.

-}

  posRef <- newIORef pos
  sizeRef <- newIORef 0

  void $ onWidgetSizeAllocate p $ \r ->
    do
      w <- rectangleReadWidth r
      h <- rectangleReadHeight r
      oldSz <- readIORef sizeRef
      oldPos <- readIORef posRef

      let sz = case o of
            Horizontal -> w
            Vertical -> h
      writeIORef sizeRef sz
      when (sz /= 0) $
        if sz == oldSz
        then do -- the slider was moved; store its new position
          sliderPos <- getPanedPosition p
          let newPos = fromIntegral sliderPos / fromIntegral sz
          writeIORef posRef newPos
          when (oldPos /= newPos) $ handleNewPos newPos
        else -- the size was changed; restore the slider position and
             -- save the new position
          set p [ panedPosition := round (oldPos * fromIntegral sz) ]

  unsafeCastTo SlidingPair p

----------------------------- LayoutDisplay
-- | A container implements 'Layout's.
data LayoutDisplay
  = LD {
     mainWidget :: Bin,
     implWidget :: IORef (Maybe LayoutImpl),
     dividerCallbacks :: IORef [DividerRef -> DividerPosition -> IO ()]
     }

-- | Tree mirroring 'Layout', which holds the layout widgets for 'LayoutDisplay'
data LayoutImpl
  = SingleWindowI {
      singleWidget :: Widget
    }
  | StackI {
      orientationI :: Orientation,
      winsI :: [(LayoutImpl, RelativeSize)],
      stackWidget :: WeightedStack
    }
  | PairI {
      orientationI :: Orientation,
      pairFstI :: LayoutImpl,
      pairSndI :: LayoutImpl,
      divRefI :: DividerRef,
      pairWidget :: SlidingPair
    }

--- construction
layoutDisplayNew :: IO LayoutDisplay
layoutDisplayNew = do
  cbRef <- newIORef []
  implRef <- newIORef Nothing
  box <- alignmentNew 0 0 1 1 >>= toBin
  return (LD box implRef cbRef)

-- | Registers a callback to a divider changing position. (There is
-- currently no way to unregister.)
layoutDisplayOnDividerMove :: LayoutDisplay
                           -> (DividerRef -> DividerPosition -> IO ())
                           -> IO ()
layoutDisplayOnDividerMove ld cb = modifyIORef (dividerCallbacks ld) (cb:)

--- changing the layout

-- | Sets the layout to the given schema.
--
-- * it is permissible to add or remove widgets in this process.
--
-- * as an optimisation, this function will first check whether the
-- layout has actually changed (so the caller need not be concerned
-- with this)
--
-- * will run 'widgetShowAll', and hence will show the underlying widgets too
layoutDisplaySet :: LayoutDisplay -> Layout Widget -> IO ()
layoutDisplaySet ld lyt = do
  mimpl <- readIORef (implWidget ld)

  let applyLayout = do
        impl' <- buildImpl (runCb $ dividerCallbacks ld) lyt
        widgetShowAll =<< outerWidget impl'
        set (mainWidget ld) [containerChild :=> outerWidget impl']
        writeIORef (implWidget ld) (Just impl')

  case mimpl of
    Nothing -> applyLayout
    Just impl -> unless (sameLayout impl lyt) $ do
      container <- toContainer $ mainWidget ld
      unattachWidgets container impl
      applyLayout

runCb :: IORef [DividerRef -> DividerPosition -> IO ()]
      -> DividerRef -> DividerPosition -> IO ()
runCb cbRef dRef dPos = readIORef cbRef >>= mapM_ (\cb -> cb dRef dPos)

buildImpl :: (DividerRef -> DividerPosition -> IO ())
          -> Layout Widget -> IO LayoutImpl
buildImpl cb = go
  where
    go (SingleWindow w) = return (SingleWindowI w)
    go (s@Stack{}) = do
      impls <- forM (wins s) $ \(lyt,relSize) -> (,relSize) <$> go lyt
      ws <- weightedStackNew (orientation s) =<< mapM (\(f, s) -> (,s) <$> outerWidget f) impls
      return (StackI (orientation s) impls ws)
    go (p@Pair{}) = do
      w1 <- go (pairFst p)
      w2 <- go (pairSnd p)
      o1 <- outerWidget w1
      o2 <- outerWidget w2
      sp <- slidingPairNew (orientation p) o1 o2
                           (divPos p) (cb $ divRef p)
      return $ PairI (orientation p) w1 w2 (divRef p) sp

-- | true if the displayed layout agrees with the given schema, other
-- than divider positions
sameLayout :: LayoutImpl -> Layout Widget -> Bool
sameLayout (SingleWindowI w) (SingleWindow w') = unsafeManagedPtrGetPtr w == unsafeManagedPtrGetPtr w'
sameLayout (s@StackI{}) (s'@Stack{}) =
     orientationI s == orientation s'
  && length (winsI s) == length (wins s')
  && and (zipWith (\(impl, relSize) (layout, relSize') ->
                    relSize == relSize' && sameLayout impl layout)
          (winsI s) (wins s'))
sameLayout (p@PairI{}) (p'@Pair{}) =
     orientationI p == orientation p'
  && divRefI p == divRef p'
  && sameLayout (pairFstI p) (pairFst p')
  && sameLayout (pairSndI p) (pairSnd p')
sameLayout _ _ = False

-- removes all widgets from the layout
unattachWidgets :: Container -> LayoutImpl -> IO ()
unattachWidgets parent (SingleWindowI w) = containerRemove parent w
unattachWidgets parent s@StackI{} = do
  containerRemove parent (stackWidget s)
  container <- toContainer $ stackWidget s
  mapM_ (unattachWidgets container . fst) (winsI s)
unattachWidgets parent p@PairI{} = do
  containerRemove parent (pairWidget p)
  container <- toContainer $ pairWidget p
  mapM_ (unattachWidgets container) [pairFstI p, pairSndI p]


-- extract the main widget from the tree
outerWidget :: LayoutImpl -> IO Widget
outerWidget s@SingleWindowI{} = return $ singleWidget s
outerWidget s@StackI{} = toWidget $ stackWidget s
outerWidget p@PairI{} = toWidget $ pairWidget p

instance WidgetLike LayoutDisplay where
  baseWidget = toWidget . mainWidget

---------------- MiniwindowDisplay
data MiniwindowDisplay
  = MD
   { mwdMainWidget :: VBox,
     mwdWidgets :: IORef [Widget]
   }

miniwindowDisplayNew :: IO MiniwindowDisplay
miniwindowDisplayNew = do
  vb <- vBoxNew False 1
  wsRef <- newIORef []
  return (MD vb wsRef)

instance WidgetLike MiniwindowDisplay where
  baseWidget = toWidget . mwdMainWidget

miniwindowDisplaySet :: MiniwindowDisplay -> [Widget] -> IO ()
miniwindowDisplaySet mwd ws = do
  curWs <- readIORef (mwdWidgets mwd)

  -- we could be more careful here, and only remove the widgets which we need to.
  when (map unsafeManagedPtrGetPtr ws /= map unsafeManagedPtrGetPtr curWs) $ do
    forM_ curWs $ containerRemove (mwdMainWidget mwd)
    forM_ ws $ \w -> boxPackEnd (mwdMainWidget mwd) w False False 0
    widgetShowAll $ mwdMainWidget mwd
    writeIORef (mwdWidgets mwd) ws


---------------------- SimpleNotebook
data SimpleNotebook
   = SN
    { snMainWidget :: Notebook,
      snTabs :: IORef (Maybe (PL.PointedList (Widget, T.Text)))
    }

instance WidgetLike SimpleNotebook where
  baseWidget = toWidget . snMainWidget

-- | Constructs an empty notebook
simpleNotebookNew :: IO SimpleNotebook
simpleNotebookNew = do
  nb <- notebookNew
  ts <- newIORef Nothing
  return (SN nb ts)

-- | Sets the tabs
simpleNotebookSet :: SimpleNotebook -> PL.PointedList (Widget, T.Text) -> IO ()
simpleNotebookSet sn ts = do
  curTs <- readIORef (snTabs sn)

  let nb = snMainWidget sn
      tsList = toList ts
      curTsList = maybe [] toList curTs
      changed = case curTs of
                    Just cts -> fmap (first unsafeManagedPtrGetPtr) cts /= fmap (first unsafeManagedPtrGetPtr) ts
                    Nothing  -> True

  -- the common case is no change at all
  when changed $ do

    -- update the tabs, if they have changed
    when (fmap (unsafeManagedPtrGetPtr . fst) curTsList /= fmap (unsafeManagedPtrGetPtr . fst) tsList) $ do
      forM_ curTsList $ const (notebookRemovePage nb (-1))
      forM_ tsList $ \(w,s) -> notebookAppendPage nb w =<< Just <$> labelNew (Just s)

    -- now update the titles if they have changed
    forM_ tsList $ \(w,s) ->
        nullToNothing (notebookGetTabLabel nb w) >>= fmap join . mapM (castTo Label) >>= \case
            Just l  -> update l (getLabelLabel, setLabelLabel) s
            Nothing -> error "Missing label widget in simpleNotebookSet"

    -- now set the focus
    p <- notebookPageNum nb (fst $ PL._focus ts)
    update nb (getNotebookPage, setNotebookPage) p

    -- write the new status
    writeIORef (snTabs sn) (Just ts)

    -- display!
    widgetShowAll nb


-- | The 'onSwitchPage' callback
simpleNotebookOnSwitchPage :: SimpleNotebook -> (Widget -> Word32 -> IO ()) -> IO ()
simpleNotebookOnSwitchPage sn = void . onNotebookSwitchPage (snMainWidget sn)


------------------- Utils
-- Only set an attribute if has actually changed.
-- This makes setting window titles much faster.
update :: (Eq a) => o -> (o -> IO a, o -> a -> IO ()) -> a -> IO ()
update w (get, set) val = do oldVal <- get w
                             when (val /= oldVal) $ set w val




















