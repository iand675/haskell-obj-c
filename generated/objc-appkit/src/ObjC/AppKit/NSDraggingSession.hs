{-# LANGUAGE PatternSynonyms #-}
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
  , enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector
  , draggingFormationSelector
  , setDraggingFormationSelector
  , animatesToStartingPositionsOnCancelOrFailSelector
  , setAnimatesToStartingPositionsOnCancelOrFailSelector
  , draggingLeaderIndexSelector
  , setDraggingLeaderIndexSelector
  , draggingPasteboardSelector
  , draggingSequenceNumberSelector
  , draggingLocationSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:@
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock :: (IsNSDraggingSession nsDraggingSession, IsNSView view, IsNSArray classArray, IsNSDictionary searchOptions) => nsDraggingSession -> NSDraggingItemEnumerationOptions -> view -> classArray -> searchOptions -> Ptr () -> IO ()
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock nsDraggingSession  enumOpts view classArray searchOptions block =
withObjCPtr view $ \raw_view ->
  withObjCPtr classArray $ \raw_classArray ->
    withObjCPtr searchOptions $ \raw_searchOptions ->
        sendMsg nsDraggingSession (mkSelector "enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:") retVoid [argCULong (coerce enumOpts), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_classArray :: Ptr ()), argPtr (castPtr raw_searchOptions :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- draggingFormation@
draggingFormation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO NSDraggingFormation
draggingFormation nsDraggingSession  =
  fmap (coerce :: CLong -> NSDraggingFormation) $ sendMsg nsDraggingSession (mkSelector "draggingFormation") retCLong []

-- | @- setDraggingFormation:@
setDraggingFormation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> NSDraggingFormation -> IO ()
setDraggingFormation nsDraggingSession  value =
  sendMsg nsDraggingSession (mkSelector "setDraggingFormation:") retVoid [argCLong (coerce value)]

-- | @- animatesToStartingPositionsOnCancelOrFail@
animatesToStartingPositionsOnCancelOrFail :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO Bool
animatesToStartingPositionsOnCancelOrFail nsDraggingSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDraggingSession (mkSelector "animatesToStartingPositionsOnCancelOrFail") retCULong []

-- | @- setAnimatesToStartingPositionsOnCancelOrFail:@
setAnimatesToStartingPositionsOnCancelOrFail :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> Bool -> IO ()
setAnimatesToStartingPositionsOnCancelOrFail nsDraggingSession  value =
  sendMsg nsDraggingSession (mkSelector "setAnimatesToStartingPositionsOnCancelOrFail:") retVoid [argCULong (if value then 1 else 0)]

-- | @- draggingLeaderIndex@
draggingLeaderIndex :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO CLong
draggingLeaderIndex nsDraggingSession  =
  sendMsg nsDraggingSession (mkSelector "draggingLeaderIndex") retCLong []

-- | @- setDraggingLeaderIndex:@
setDraggingLeaderIndex :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> CLong -> IO ()
setDraggingLeaderIndex nsDraggingSession  value =
  sendMsg nsDraggingSession (mkSelector "setDraggingLeaderIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- draggingPasteboard@
draggingPasteboard :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO (Id NSPasteboard)
draggingPasteboard nsDraggingSession  =
  sendMsg nsDraggingSession (mkSelector "draggingPasteboard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- draggingSequenceNumber@
draggingSequenceNumber :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO CLong
draggingSequenceNumber nsDraggingSession  =
  sendMsg nsDraggingSession (mkSelector "draggingSequenceNumber") retCLong []

-- | @- draggingLocation@
draggingLocation :: IsNSDraggingSession nsDraggingSession => nsDraggingSession -> IO NSPoint
draggingLocation nsDraggingSession  =
  sendMsgStret nsDraggingSession (mkSelector "draggingLocation") retNSPoint []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:@
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector :: Selector
enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlockSelector = mkSelector "enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:"

-- | @Selector@ for @draggingFormation@
draggingFormationSelector :: Selector
draggingFormationSelector = mkSelector "draggingFormation"

-- | @Selector@ for @setDraggingFormation:@
setDraggingFormationSelector :: Selector
setDraggingFormationSelector = mkSelector "setDraggingFormation:"

-- | @Selector@ for @animatesToStartingPositionsOnCancelOrFail@
animatesToStartingPositionsOnCancelOrFailSelector :: Selector
animatesToStartingPositionsOnCancelOrFailSelector = mkSelector "animatesToStartingPositionsOnCancelOrFail"

-- | @Selector@ for @setAnimatesToStartingPositionsOnCancelOrFail:@
setAnimatesToStartingPositionsOnCancelOrFailSelector :: Selector
setAnimatesToStartingPositionsOnCancelOrFailSelector = mkSelector "setAnimatesToStartingPositionsOnCancelOrFail:"

-- | @Selector@ for @draggingLeaderIndex@
draggingLeaderIndexSelector :: Selector
draggingLeaderIndexSelector = mkSelector "draggingLeaderIndex"

-- | @Selector@ for @setDraggingLeaderIndex:@
setDraggingLeaderIndexSelector :: Selector
setDraggingLeaderIndexSelector = mkSelector "setDraggingLeaderIndex:"

-- | @Selector@ for @draggingPasteboard@
draggingPasteboardSelector :: Selector
draggingPasteboardSelector = mkSelector "draggingPasteboard"

-- | @Selector@ for @draggingSequenceNumber@
draggingSequenceNumberSelector :: Selector
draggingSequenceNumberSelector = mkSelector "draggingSequenceNumber"

-- | @Selector@ for @draggingLocation@
draggingLocationSelector :: Selector
draggingLocationSelector = mkSelector "draggingLocation"

