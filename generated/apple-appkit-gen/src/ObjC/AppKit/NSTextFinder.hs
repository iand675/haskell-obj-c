{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextFinder@.
module ObjC.AppKit.NSTextFinder
  ( NSTextFinder
  , IsNSTextFinder(..)
  , init_
  , initWithCoder
  , performAction
  , validateAction
  , cancelFindIndicator
  , drawIncrementalMatchHighlightInRect
  , noteClientStringWillChange
  , client
  , setClient
  , findBarContainer
  , setFindBarContainer
  , findIndicatorNeedsUpdate
  , setFindIndicatorNeedsUpdate
  , incrementalSearchingEnabled
  , setIncrementalSearchingEnabled
  , incrementalSearchingShouldDimContentView
  , setIncrementalSearchingShouldDimContentView
  , incrementalMatchRanges
  , initSelector
  , initWithCoderSelector
  , performActionSelector
  , validateActionSelector
  , cancelFindIndicatorSelector
  , drawIncrementalMatchHighlightInRectSelector
  , noteClientStringWillChangeSelector
  , clientSelector
  , setClientSelector
  , findBarContainerSelector
  , setFindBarContainerSelector
  , findIndicatorNeedsUpdateSelector
  , setFindIndicatorNeedsUpdateSelector
  , incrementalSearchingEnabledSelector
  , setIncrementalSearchingEnabledSelector
  , incrementalSearchingShouldDimContentViewSelector
  , setIncrementalSearchingShouldDimContentViewSelector
  , incrementalMatchRangesSelector

  -- * Enum types
  , NSTextFinderAction(NSTextFinderAction)
  , pattern NSTextFinderActionShowFindInterface
  , pattern NSTextFinderActionNextMatch
  , pattern NSTextFinderActionPreviousMatch
  , pattern NSTextFinderActionReplaceAll
  , pattern NSTextFinderActionReplace
  , pattern NSTextFinderActionReplaceAndFind
  , pattern NSTextFinderActionSetSearchString
  , pattern NSTextFinderActionReplaceAllInSelection
  , pattern NSTextFinderActionSelectAll
  , pattern NSTextFinderActionSelectAllInSelection
  , pattern NSTextFinderActionHideFindInterface
  , pattern NSTextFinderActionShowReplaceInterface
  , pattern NSTextFinderActionHideReplaceInterface

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO (Id NSTextFinder)
init_ nsTextFinder  =
    sendMsg nsTextFinder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextFinder nsTextFinder, IsNSCoder coder) => nsTextFinder -> coder -> IO (Id NSTextFinder)
initWithCoder nsTextFinder  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsTextFinder (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- performAction:@
performAction :: IsNSTextFinder nsTextFinder => nsTextFinder -> NSTextFinderAction -> IO ()
performAction nsTextFinder  op =
    sendMsg nsTextFinder (mkSelector "performAction:") retVoid [argCLong (coerce op)]

-- | @- validateAction:@
validateAction :: IsNSTextFinder nsTextFinder => nsTextFinder -> NSTextFinderAction -> IO Bool
validateAction nsTextFinder  op =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextFinder (mkSelector "validateAction:") retCULong [argCLong (coerce op)]

-- | @- cancelFindIndicator@
cancelFindIndicator :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO ()
cancelFindIndicator nsTextFinder  =
    sendMsg nsTextFinder (mkSelector "cancelFindIndicator") retVoid []

-- | @+ drawIncrementalMatchHighlightInRect:@
drawIncrementalMatchHighlightInRect :: NSRect -> IO ()
drawIncrementalMatchHighlightInRect rect =
  do
    cls' <- getRequiredClass "NSTextFinder"
    sendClassMsg cls' (mkSelector "drawIncrementalMatchHighlightInRect:") retVoid [argNSRect rect]

-- | @- noteClientStringWillChange@
noteClientStringWillChange :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO ()
noteClientStringWillChange nsTextFinder  =
    sendMsg nsTextFinder (mkSelector "noteClientStringWillChange") retVoid []

-- | @- client@
client :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO RawId
client nsTextFinder  =
    fmap (RawId . castPtr) $ sendMsg nsTextFinder (mkSelector "client") (retPtr retVoid) []

-- | @- setClient:@
setClient :: IsNSTextFinder nsTextFinder => nsTextFinder -> RawId -> IO ()
setClient nsTextFinder  value =
    sendMsg nsTextFinder (mkSelector "setClient:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- findBarContainer@
findBarContainer :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO RawId
findBarContainer nsTextFinder  =
    fmap (RawId . castPtr) $ sendMsg nsTextFinder (mkSelector "findBarContainer") (retPtr retVoid) []

-- | @- setFindBarContainer:@
setFindBarContainer :: IsNSTextFinder nsTextFinder => nsTextFinder -> RawId -> IO ()
setFindBarContainer nsTextFinder  value =
    sendMsg nsTextFinder (mkSelector "setFindBarContainer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- findIndicatorNeedsUpdate@
findIndicatorNeedsUpdate :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
findIndicatorNeedsUpdate nsTextFinder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextFinder (mkSelector "findIndicatorNeedsUpdate") retCULong []

-- | @- setFindIndicatorNeedsUpdate:@
setFindIndicatorNeedsUpdate :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setFindIndicatorNeedsUpdate nsTextFinder  value =
    sendMsg nsTextFinder (mkSelector "setFindIndicatorNeedsUpdate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- incrementalSearchingEnabled@
incrementalSearchingEnabled :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
incrementalSearchingEnabled nsTextFinder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextFinder (mkSelector "incrementalSearchingEnabled") retCULong []

-- | @- setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabled :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setIncrementalSearchingEnabled nsTextFinder  value =
    sendMsg nsTextFinder (mkSelector "setIncrementalSearchingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- incrementalSearchingShouldDimContentView@
incrementalSearchingShouldDimContentView :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
incrementalSearchingShouldDimContentView nsTextFinder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextFinder (mkSelector "incrementalSearchingShouldDimContentView") retCULong []

-- | @- setIncrementalSearchingShouldDimContentView:@
setIncrementalSearchingShouldDimContentView :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setIncrementalSearchingShouldDimContentView nsTextFinder  value =
    sendMsg nsTextFinder (mkSelector "setIncrementalSearchingShouldDimContentView:") retVoid [argCULong (if value then 1 else 0)]

-- | @- incrementalMatchRanges@
incrementalMatchRanges :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO (Id NSArray)
incrementalMatchRanges nsTextFinder  =
    sendMsg nsTextFinder (mkSelector "incrementalMatchRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @performAction:@
performActionSelector :: Selector
performActionSelector = mkSelector "performAction:"

-- | @Selector@ for @validateAction:@
validateActionSelector :: Selector
validateActionSelector = mkSelector "validateAction:"

-- | @Selector@ for @cancelFindIndicator@
cancelFindIndicatorSelector :: Selector
cancelFindIndicatorSelector = mkSelector "cancelFindIndicator"

-- | @Selector@ for @drawIncrementalMatchHighlightInRect:@
drawIncrementalMatchHighlightInRectSelector :: Selector
drawIncrementalMatchHighlightInRectSelector = mkSelector "drawIncrementalMatchHighlightInRect:"

-- | @Selector@ for @noteClientStringWillChange@
noteClientStringWillChangeSelector :: Selector
noteClientStringWillChangeSelector = mkSelector "noteClientStringWillChange"

-- | @Selector@ for @client@
clientSelector :: Selector
clientSelector = mkSelector "client"

-- | @Selector@ for @setClient:@
setClientSelector :: Selector
setClientSelector = mkSelector "setClient:"

-- | @Selector@ for @findBarContainer@
findBarContainerSelector :: Selector
findBarContainerSelector = mkSelector "findBarContainer"

-- | @Selector@ for @setFindBarContainer:@
setFindBarContainerSelector :: Selector
setFindBarContainerSelector = mkSelector "setFindBarContainer:"

-- | @Selector@ for @findIndicatorNeedsUpdate@
findIndicatorNeedsUpdateSelector :: Selector
findIndicatorNeedsUpdateSelector = mkSelector "findIndicatorNeedsUpdate"

-- | @Selector@ for @setFindIndicatorNeedsUpdate:@
setFindIndicatorNeedsUpdateSelector :: Selector
setFindIndicatorNeedsUpdateSelector = mkSelector "setFindIndicatorNeedsUpdate:"

-- | @Selector@ for @incrementalSearchingEnabled@
incrementalSearchingEnabledSelector :: Selector
incrementalSearchingEnabledSelector = mkSelector "incrementalSearchingEnabled"

-- | @Selector@ for @setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabledSelector :: Selector
setIncrementalSearchingEnabledSelector = mkSelector "setIncrementalSearchingEnabled:"

-- | @Selector@ for @incrementalSearchingShouldDimContentView@
incrementalSearchingShouldDimContentViewSelector :: Selector
incrementalSearchingShouldDimContentViewSelector = mkSelector "incrementalSearchingShouldDimContentView"

-- | @Selector@ for @setIncrementalSearchingShouldDimContentView:@
setIncrementalSearchingShouldDimContentViewSelector :: Selector
setIncrementalSearchingShouldDimContentViewSelector = mkSelector "setIncrementalSearchingShouldDimContentView:"

-- | @Selector@ for @incrementalMatchRanges@
incrementalMatchRangesSelector :: Selector
incrementalMatchRangesSelector = mkSelector "incrementalMatchRanges"

