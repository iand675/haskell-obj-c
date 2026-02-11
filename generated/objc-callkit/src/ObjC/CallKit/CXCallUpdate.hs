{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallUpdate@.
module ObjC.CallKit.CXCallUpdate
  ( CXCallUpdate
  , IsCXCallUpdate(..)
  , remoteHandle
  , setRemoteHandle
  , localizedCallerName
  , setLocalizedCallerName
  , supportsHolding
  , setSupportsHolding
  , supportsGrouping
  , setSupportsGrouping
  , supportsUngrouping
  , setSupportsUngrouping
  , supportsDTMF
  , setSupportsDTMF
  , hasVideo
  , setHasVideo
  , remoteHandleSelector
  , setRemoteHandleSelector
  , localizedCallerNameSelector
  , setLocalizedCallerNameSelector
  , supportsHoldingSelector
  , setSupportsHoldingSelector
  , supportsGroupingSelector
  , setSupportsGroupingSelector
  , supportsUngroupingSelector
  , setSupportsUngroupingSelector
  , supportsDTMFSelector
  , setSupportsDTMFSelector
  , hasVideoSelector
  , setHasVideoSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Handle for the remote party (for an incoming call, the caller; for an outgoing call, the callee).
--
-- ObjC selector: @- remoteHandle@
remoteHandle :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO (Id CXHandle)
remoteHandle cxCallUpdate  =
  sendMsg cxCallUpdate (mkSelector "remoteHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Handle for the remote party (for an incoming call, the caller; for an outgoing call, the callee).
--
-- ObjC selector: @- setRemoteHandle:@
setRemoteHandle :: (IsCXCallUpdate cxCallUpdate, IsCXHandle value) => cxCallUpdate -> value -> IO ()
setRemoteHandle cxCallUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxCallUpdate (mkSelector "setRemoteHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Override the computed caller name to a provider-defined value. Normally the system will determine the appropriate caller name to display (e.g. using the user's contacts) based on the supplied caller identifier. Set this property to customize.
--
-- ObjC selector: @- localizedCallerName@
localizedCallerName :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO (Id NSString)
localizedCallerName cxCallUpdate  =
  sendMsg cxCallUpdate (mkSelector "localizedCallerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Override the computed caller name to a provider-defined value. Normally the system will determine the appropriate caller name to display (e.g. using the user's contacts) based on the supplied caller identifier. Set this property to customize.
--
-- ObjC selector: @- setLocalizedCallerName:@
setLocalizedCallerName :: (IsCXCallUpdate cxCallUpdate, IsNSString value) => cxCallUpdate -> value -> IO ()
setLocalizedCallerName cxCallUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxCallUpdate (mkSelector "setLocalizedCallerName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether the call can be held on its own or swapped with another call
--
-- ObjC selector: @- supportsHolding@
supportsHolding :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsHolding cxCallUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallUpdate (mkSelector "supportsHolding") retCULong []

-- | Whether the call can be held on its own or swapped with another call
--
-- ObjC selector: @- setSupportsHolding:@
setSupportsHolding :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsHolding cxCallUpdate  value =
  sendMsg cxCallUpdate (mkSelector "setSupportsHolding:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether the call can be grouped (merged) with other calls when it is ungrouped
--
-- ObjC selector: @- supportsGrouping@
supportsGrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsGrouping cxCallUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallUpdate (mkSelector "supportsGrouping") retCULong []

-- | Whether the call can be grouped (merged) with other calls when it is ungrouped
--
-- ObjC selector: @- setSupportsGrouping:@
setSupportsGrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsGrouping cxCallUpdate  value =
  sendMsg cxCallUpdate (mkSelector "setSupportsGrouping:") retVoid [argCULong (if value then 1 else 0)]

-- | The call can be ungrouped (taken private) when it is grouped
--
-- ObjC selector: @- supportsUngrouping@
supportsUngrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsUngrouping cxCallUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallUpdate (mkSelector "supportsUngrouping") retCULong []

-- | The call can be ungrouped (taken private) when it is grouped
--
-- ObjC selector: @- setSupportsUngrouping:@
setSupportsUngrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsUngrouping cxCallUpdate  value =
  sendMsg cxCallUpdate (mkSelector "setSupportsUngrouping:") retVoid [argCULong (if value then 1 else 0)]

-- | The call can send DTMF tones via hard pause digits or in-call keypad entries
--
-- ObjC selector: @- supportsDTMF@
supportsDTMF :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsDTMF cxCallUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallUpdate (mkSelector "supportsDTMF") retCULong []

-- | The call can send DTMF tones via hard pause digits or in-call keypad entries
--
-- ObjC selector: @- setSupportsDTMF:@
setSupportsDTMF :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsDTMF cxCallUpdate  value =
  sendMsg cxCallUpdate (mkSelector "setSupportsDTMF:") retVoid [argCULong (if value then 1 else 0)]

-- | The call includes video in addition to audio.
--
-- ObjC selector: @- hasVideo@
hasVideo :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
hasVideo cxCallUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallUpdate (mkSelector "hasVideo") retCULong []

-- | The call includes video in addition to audio.
--
-- ObjC selector: @- setHasVideo:@
setHasVideo :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setHasVideo cxCallUpdate  value =
  sendMsg cxCallUpdate (mkSelector "setHasVideo:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remoteHandle@
remoteHandleSelector :: Selector
remoteHandleSelector = mkSelector "remoteHandle"

-- | @Selector@ for @setRemoteHandle:@
setRemoteHandleSelector :: Selector
setRemoteHandleSelector = mkSelector "setRemoteHandle:"

-- | @Selector@ for @localizedCallerName@
localizedCallerNameSelector :: Selector
localizedCallerNameSelector = mkSelector "localizedCallerName"

-- | @Selector@ for @setLocalizedCallerName:@
setLocalizedCallerNameSelector :: Selector
setLocalizedCallerNameSelector = mkSelector "setLocalizedCallerName:"

-- | @Selector@ for @supportsHolding@
supportsHoldingSelector :: Selector
supportsHoldingSelector = mkSelector "supportsHolding"

-- | @Selector@ for @setSupportsHolding:@
setSupportsHoldingSelector :: Selector
setSupportsHoldingSelector = mkSelector "setSupportsHolding:"

-- | @Selector@ for @supportsGrouping@
supportsGroupingSelector :: Selector
supportsGroupingSelector = mkSelector "supportsGrouping"

-- | @Selector@ for @setSupportsGrouping:@
setSupportsGroupingSelector :: Selector
setSupportsGroupingSelector = mkSelector "setSupportsGrouping:"

-- | @Selector@ for @supportsUngrouping@
supportsUngroupingSelector :: Selector
supportsUngroupingSelector = mkSelector "supportsUngrouping"

-- | @Selector@ for @setSupportsUngrouping:@
setSupportsUngroupingSelector :: Selector
setSupportsUngroupingSelector = mkSelector "setSupportsUngrouping:"

-- | @Selector@ for @supportsDTMF@
supportsDTMFSelector :: Selector
supportsDTMFSelector = mkSelector "supportsDTMF"

-- | @Selector@ for @setSupportsDTMF:@
setSupportsDTMFSelector :: Selector
setSupportsDTMFSelector = mkSelector "setSupportsDTMF:"

-- | @Selector@ for @hasVideo@
hasVideoSelector :: Selector
hasVideoSelector = mkSelector "hasVideo"

-- | @Selector@ for @setHasVideo:@
setHasVideoSelector :: Selector
setHasVideoSelector = mkSelector "setHasVideo:"

