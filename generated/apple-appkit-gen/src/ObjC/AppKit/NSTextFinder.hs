{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cancelFindIndicatorSelector
  , clientSelector
  , drawIncrementalMatchHighlightInRectSelector
  , findBarContainerSelector
  , findIndicatorNeedsUpdateSelector
  , incrementalMatchRangesSelector
  , incrementalSearchingEnabledSelector
  , incrementalSearchingShouldDimContentViewSelector
  , initSelector
  , initWithCoderSelector
  , noteClientStringWillChangeSelector
  , performActionSelector
  , setClientSelector
  , setFindBarContainerSelector
  , setFindIndicatorNeedsUpdateSelector
  , setIncrementalSearchingEnabledSelector
  , setIncrementalSearchingShouldDimContentViewSelector
  , validateActionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO (Id NSTextFinder)
init_ nsTextFinder =
  sendOwnedMessage nsTextFinder initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextFinder nsTextFinder, IsNSCoder coder) => nsTextFinder -> coder -> IO (Id NSTextFinder)
initWithCoder nsTextFinder coder =
  sendOwnedMessage nsTextFinder initWithCoderSelector (toNSCoder coder)

-- | @- performAction:@
performAction :: IsNSTextFinder nsTextFinder => nsTextFinder -> NSTextFinderAction -> IO ()
performAction nsTextFinder op =
  sendMessage nsTextFinder performActionSelector op

-- | @- validateAction:@
validateAction :: IsNSTextFinder nsTextFinder => nsTextFinder -> NSTextFinderAction -> IO Bool
validateAction nsTextFinder op =
  sendMessage nsTextFinder validateActionSelector op

-- | @- cancelFindIndicator@
cancelFindIndicator :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO ()
cancelFindIndicator nsTextFinder =
  sendMessage nsTextFinder cancelFindIndicatorSelector

-- | @+ drawIncrementalMatchHighlightInRect:@
drawIncrementalMatchHighlightInRect :: NSRect -> IO ()
drawIncrementalMatchHighlightInRect rect =
  do
    cls' <- getRequiredClass "NSTextFinder"
    sendClassMessage cls' drawIncrementalMatchHighlightInRectSelector rect

-- | @- noteClientStringWillChange@
noteClientStringWillChange :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO ()
noteClientStringWillChange nsTextFinder =
  sendMessage nsTextFinder noteClientStringWillChangeSelector

-- | @- client@
client :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO RawId
client nsTextFinder =
  sendMessage nsTextFinder clientSelector

-- | @- setClient:@
setClient :: IsNSTextFinder nsTextFinder => nsTextFinder -> RawId -> IO ()
setClient nsTextFinder value =
  sendMessage nsTextFinder setClientSelector value

-- | @- findBarContainer@
findBarContainer :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO RawId
findBarContainer nsTextFinder =
  sendMessage nsTextFinder findBarContainerSelector

-- | @- setFindBarContainer:@
setFindBarContainer :: IsNSTextFinder nsTextFinder => nsTextFinder -> RawId -> IO ()
setFindBarContainer nsTextFinder value =
  sendMessage nsTextFinder setFindBarContainerSelector value

-- | @- findIndicatorNeedsUpdate@
findIndicatorNeedsUpdate :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
findIndicatorNeedsUpdate nsTextFinder =
  sendMessage nsTextFinder findIndicatorNeedsUpdateSelector

-- | @- setFindIndicatorNeedsUpdate:@
setFindIndicatorNeedsUpdate :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setFindIndicatorNeedsUpdate nsTextFinder value =
  sendMessage nsTextFinder setFindIndicatorNeedsUpdateSelector value

-- | @- incrementalSearchingEnabled@
incrementalSearchingEnabled :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
incrementalSearchingEnabled nsTextFinder =
  sendMessage nsTextFinder incrementalSearchingEnabledSelector

-- | @- setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabled :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setIncrementalSearchingEnabled nsTextFinder value =
  sendMessage nsTextFinder setIncrementalSearchingEnabledSelector value

-- | @- incrementalSearchingShouldDimContentView@
incrementalSearchingShouldDimContentView :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO Bool
incrementalSearchingShouldDimContentView nsTextFinder =
  sendMessage nsTextFinder incrementalSearchingShouldDimContentViewSelector

-- | @- setIncrementalSearchingShouldDimContentView:@
setIncrementalSearchingShouldDimContentView :: IsNSTextFinder nsTextFinder => nsTextFinder -> Bool -> IO ()
setIncrementalSearchingShouldDimContentView nsTextFinder value =
  sendMessage nsTextFinder setIncrementalSearchingShouldDimContentViewSelector value

-- | @- incrementalMatchRanges@
incrementalMatchRanges :: IsNSTextFinder nsTextFinder => nsTextFinder -> IO (Id NSArray)
incrementalMatchRanges nsTextFinder =
  sendMessage nsTextFinder incrementalMatchRangesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextFinder)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextFinder)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @performAction:@
performActionSelector :: Selector '[NSTextFinderAction] ()
performActionSelector = mkSelector "performAction:"

-- | @Selector@ for @validateAction:@
validateActionSelector :: Selector '[NSTextFinderAction] Bool
validateActionSelector = mkSelector "validateAction:"

-- | @Selector@ for @cancelFindIndicator@
cancelFindIndicatorSelector :: Selector '[] ()
cancelFindIndicatorSelector = mkSelector "cancelFindIndicator"

-- | @Selector@ for @drawIncrementalMatchHighlightInRect:@
drawIncrementalMatchHighlightInRectSelector :: Selector '[NSRect] ()
drawIncrementalMatchHighlightInRectSelector = mkSelector "drawIncrementalMatchHighlightInRect:"

-- | @Selector@ for @noteClientStringWillChange@
noteClientStringWillChangeSelector :: Selector '[] ()
noteClientStringWillChangeSelector = mkSelector "noteClientStringWillChange"

-- | @Selector@ for @client@
clientSelector :: Selector '[] RawId
clientSelector = mkSelector "client"

-- | @Selector@ for @setClient:@
setClientSelector :: Selector '[RawId] ()
setClientSelector = mkSelector "setClient:"

-- | @Selector@ for @findBarContainer@
findBarContainerSelector :: Selector '[] RawId
findBarContainerSelector = mkSelector "findBarContainer"

-- | @Selector@ for @setFindBarContainer:@
setFindBarContainerSelector :: Selector '[RawId] ()
setFindBarContainerSelector = mkSelector "setFindBarContainer:"

-- | @Selector@ for @findIndicatorNeedsUpdate@
findIndicatorNeedsUpdateSelector :: Selector '[] Bool
findIndicatorNeedsUpdateSelector = mkSelector "findIndicatorNeedsUpdate"

-- | @Selector@ for @setFindIndicatorNeedsUpdate:@
setFindIndicatorNeedsUpdateSelector :: Selector '[Bool] ()
setFindIndicatorNeedsUpdateSelector = mkSelector "setFindIndicatorNeedsUpdate:"

-- | @Selector@ for @incrementalSearchingEnabled@
incrementalSearchingEnabledSelector :: Selector '[] Bool
incrementalSearchingEnabledSelector = mkSelector "incrementalSearchingEnabled"

-- | @Selector@ for @setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabledSelector :: Selector '[Bool] ()
setIncrementalSearchingEnabledSelector = mkSelector "setIncrementalSearchingEnabled:"

-- | @Selector@ for @incrementalSearchingShouldDimContentView@
incrementalSearchingShouldDimContentViewSelector :: Selector '[] Bool
incrementalSearchingShouldDimContentViewSelector = mkSelector "incrementalSearchingShouldDimContentView"

-- | @Selector@ for @setIncrementalSearchingShouldDimContentView:@
setIncrementalSearchingShouldDimContentViewSelector :: Selector '[Bool] ()
setIncrementalSearchingShouldDimContentViewSelector = mkSelector "setIncrementalSearchingShouldDimContentView:"

-- | @Selector@ for @incrementalMatchRanges@
incrementalMatchRangesSelector :: Selector '[] (Id NSArray)
incrementalMatchRangesSelector = mkSelector "incrementalMatchRanges"

