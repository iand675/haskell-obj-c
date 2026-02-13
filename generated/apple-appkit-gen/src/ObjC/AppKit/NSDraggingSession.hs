{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDraggingSession@.
module ObjC.AppKit.NSDraggingSession
  ( NSDraggingSession
  , IsNSDraggingSession(..)
  , enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock
  , draggingFormation
  , setDraggingFormation
  , animatesToStartingPositionsOnCancelOrFail
  , setAnimatesToStartingPositionsOnCancelOrFail
  , draggingLeaderIndex
  , setDraggingLeaderIndex
  , draggingPasteboard
  , draggingSequenceNumber
  , draggingLocation
  , animatesToStartingPositionsOnCancelOrFailSelector
  , draggingFormationSelector
  , draggingLeaderIndexSelector
  , draggingLocationSelector
  , draggingPasteboardSelector
  , draggingSequenceNumberSelector
  , enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector
  , setAnimatesToStartingPositionsOnCancelOrFailSelector
  , setDraggingFormationSelector
  , setDraggingLeaderIndexSelector

  -- * Enum types
  , NSDraggingFormation(NSDraggingFormation)
  , pattern NSDraggingFormationDefault
  , pattern NSDraggingFormationNone
  , pattern NSDraggingFormationPile
  , pattern NSDraggingFormationList
  , pattern NSDraggingFormationStack
  , NSDraggingItemEnumerationOptions(NSDraggingItemEnumerationOptions)
  , pattern NSDraggingItemEnumerationConcurrent
  , pattern NSDraggingItemEnumerationClearNonenumeratedImages

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

-- | @- enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:@
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock :: (IsNSDraggingSession nsDraggingSession, IsNSView view, IsNSArray classArray, IsNSDictionary searchOptions) => nsDraggingSession -> NSDraggingItemEnumerationOptions -> view -> classArray -> searchOptions -> Ptr () -> IO ()
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock nsDraggingSession enumOpts view classArray searchOptions block =
  sendMessage nsDraggingSession enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector enumOpts (toNSView view) (toNSArray classArray) (toNSDictionary searchOptions) block

-- | @- draggingFormation@
draggingFormation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO NSDraggingFormation
draggingFormation nsDraggingSession =
  sendMessage nsDraggingSession draggingFormationSelector

-- | @- setDraggingFormation:@
setDraggingFormation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> NSDraggingFormation -> IO ()
setDraggingFormation nsDraggingSession value =
  sendMessage nsDraggingSession setDraggingFormationSelector value

-- | @- animatesToStartingPositionsOnCancelOrFail@
animatesToStartingPositionsOnCancelOrFail :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO Bool
animatesToStartingPositionsOnCancelOrFail nsDraggingSession =
  sendMessage nsDraggingSession animatesToStartingPositionsOnCancelOrFailSelector

-- | @- setAnimatesToStartingPositionsOnCancelOrFail:@
setAnimatesToStartingPositionsOnCancelOrFail :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> Bool -> IO ()
setAnimatesToStartingPositionsOnCancelOrFail nsDraggingSession value =
  sendMessage nsDraggingSession setAnimatesToStartingPositionsOnCancelOrFailSelector value

-- | @- draggingLeaderIndex@
draggingLeaderIndex :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO CLong
draggingLeaderIndex nsDraggingSession =
  sendMessage nsDraggingSession draggingLeaderIndexSelector

-- | @- setDraggingLeaderIndex:@
setDraggingLeaderIndex :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> CLong -> IO ()
setDraggingLeaderIndex nsDraggingSession value =
  sendMessage nsDraggingSession setDraggingLeaderIndexSelector value

-- | @- draggingPasteboard@
draggingPasteboard :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO (Id NSPasteboard)
draggingPasteboard nsDraggingSession =
  sendMessage nsDraggingSession draggingPasteboardSelector

-- | @- draggingSequenceNumber@
draggingSequenceNumber :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO CLong
draggingSequenceNumber nsDraggingSession =
  sendMessage nsDraggingSession draggingSequenceNumberSelector

-- | @- draggingLocation@
draggingLocation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO NSPoint
draggingLocation nsDraggingSession =
  sendMessage nsDraggingSession draggingLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:@
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector :: Selector '[NSDraggingItemEnumerationOptions, Id NSView, Id NSArray, Id NSDictionary, Ptr ()] ()
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector = mkSelector "enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:"

-- | @Selector@ for @draggingFormation@
draggingFormationSelector :: Selector '[] NSDraggingFormation
draggingFormationSelector = mkSelector "draggingFormation"

-- | @Selector@ for @setDraggingFormation:@
setDraggingFormationSelector :: Selector '[NSDraggingFormation] ()
setDraggingFormationSelector = mkSelector "setDraggingFormation:"

-- | @Selector@ for @animatesToStartingPositionsOnCancelOrFail@
animatesToStartingPositionsOnCancelOrFailSelector :: Selector '[] Bool
animatesToStartingPositionsOnCancelOrFailSelector = mkSelector "animatesToStartingPositionsOnCancelOrFail"

-- | @Selector@ for @setAnimatesToStartingPositionsOnCancelOrFail:@
setAnimatesToStartingPositionsOnCancelOrFailSelector :: Selector '[Bool] ()
setAnimatesToStartingPositionsOnCancelOrFailSelector = mkSelector "setAnimatesToStartingPositionsOnCancelOrFail:"

-- | @Selector@ for @draggingLeaderIndex@
draggingLeaderIndexSelector :: Selector '[] CLong
draggingLeaderIndexSelector = mkSelector "draggingLeaderIndex"

-- | @Selector@ for @setDraggingLeaderIndex:@
setDraggingLeaderIndexSelector :: Selector '[CLong] ()
setDraggingLeaderIndexSelector = mkSelector "setDraggingLeaderIndex:"

-- | @Selector@ for @draggingPasteboard@
draggingPasteboardSelector :: Selector '[] (Id NSPasteboard)
draggingPasteboardSelector = mkSelector "draggingPasteboard"

-- | @Selector@ for @draggingSequenceNumber@
draggingSequenceNumberSelector :: Selector '[] CLong
draggingSequenceNumberSelector = mkSelector "draggingSequenceNumber"

-- | @Selector@ for @draggingLocation@
draggingLocationSelector :: Selector '[] NSPoint
draggingLocationSelector = mkSelector "draggingLocation"

