{-# LANGUAGE DataKinds #-}
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
  , hasVideoSelector
  , localizedCallerNameSelector
  , remoteHandleSelector
  , setHasVideoSelector
  , setLocalizedCallerNameSelector
  , setRemoteHandleSelector
  , setSupportsDTMFSelector
  , setSupportsGroupingSelector
  , setSupportsHoldingSelector
  , setSupportsUngroupingSelector
  , supportsDTMFSelector
  , supportsGroupingSelector
  , supportsHoldingSelector
  , supportsUngroupingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Handle for the remote party (for an incoming call, the caller; for an outgoing call, the callee).
--
-- ObjC selector: @- remoteHandle@
remoteHandle :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO (Id CXHandle)
remoteHandle cxCallUpdate =
  sendMessage cxCallUpdate remoteHandleSelector

-- | Handle for the remote party (for an incoming call, the caller; for an outgoing call, the callee).
--
-- ObjC selector: @- setRemoteHandle:@
setRemoteHandle :: (IsCXCallUpdate cxCallUpdate, IsCXHandle value) => cxCallUpdate -> value -> IO ()
setRemoteHandle cxCallUpdate value =
  sendMessage cxCallUpdate setRemoteHandleSelector (toCXHandle value)

-- | Override the computed caller name to a provider-defined value. Normally the system will determine the appropriate caller name to display (e.g. using the user's contacts) based on the supplied caller identifier. Set this property to customize.
--
-- ObjC selector: @- localizedCallerName@
localizedCallerName :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO (Id NSString)
localizedCallerName cxCallUpdate =
  sendMessage cxCallUpdate localizedCallerNameSelector

-- | Override the computed caller name to a provider-defined value. Normally the system will determine the appropriate caller name to display (e.g. using the user's contacts) based on the supplied caller identifier. Set this property to customize.
--
-- ObjC selector: @- setLocalizedCallerName:@
setLocalizedCallerName :: (IsCXCallUpdate cxCallUpdate, IsNSString value) => cxCallUpdate -> value -> IO ()
setLocalizedCallerName cxCallUpdate value =
  sendMessage cxCallUpdate setLocalizedCallerNameSelector (toNSString value)

-- | Whether the call can be held on its own or swapped with another call
--
-- ObjC selector: @- supportsHolding@
supportsHolding :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsHolding cxCallUpdate =
  sendMessage cxCallUpdate supportsHoldingSelector

-- | Whether the call can be held on its own or swapped with another call
--
-- ObjC selector: @- setSupportsHolding:@
setSupportsHolding :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsHolding cxCallUpdate value =
  sendMessage cxCallUpdate setSupportsHoldingSelector value

-- | Whether the call can be grouped (merged) with other calls when it is ungrouped
--
-- ObjC selector: @- supportsGrouping@
supportsGrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsGrouping cxCallUpdate =
  sendMessage cxCallUpdate supportsGroupingSelector

-- | Whether the call can be grouped (merged) with other calls when it is ungrouped
--
-- ObjC selector: @- setSupportsGrouping:@
setSupportsGrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsGrouping cxCallUpdate value =
  sendMessage cxCallUpdate setSupportsGroupingSelector value

-- | The call can be ungrouped (taken private) when it is grouped
--
-- ObjC selector: @- supportsUngrouping@
supportsUngrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsUngrouping cxCallUpdate =
  sendMessage cxCallUpdate supportsUngroupingSelector

-- | The call can be ungrouped (taken private) when it is grouped
--
-- ObjC selector: @- setSupportsUngrouping:@
setSupportsUngrouping :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsUngrouping cxCallUpdate value =
  sendMessage cxCallUpdate setSupportsUngroupingSelector value

-- | The call can send DTMF tones via hard pause digits or in-call keypad entries
--
-- ObjC selector: @- supportsDTMF@
supportsDTMF :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
supportsDTMF cxCallUpdate =
  sendMessage cxCallUpdate supportsDTMFSelector

-- | The call can send DTMF tones via hard pause digits or in-call keypad entries
--
-- ObjC selector: @- setSupportsDTMF:@
setSupportsDTMF :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setSupportsDTMF cxCallUpdate value =
  sendMessage cxCallUpdate setSupportsDTMFSelector value

-- | The call includes video in addition to audio.
--
-- ObjC selector: @- hasVideo@
hasVideo :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> IO Bool
hasVideo cxCallUpdate =
  sendMessage cxCallUpdate hasVideoSelector

-- | The call includes video in addition to audio.
--
-- ObjC selector: @- setHasVideo:@
setHasVideo :: IsCXCallUpdate cxCallUpdate => cxCallUpdate -> Bool -> IO ()
setHasVideo cxCallUpdate value =
  sendMessage cxCallUpdate setHasVideoSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remoteHandle@
remoteHandleSelector :: Selector '[] (Id CXHandle)
remoteHandleSelector = mkSelector "remoteHandle"

-- | @Selector@ for @setRemoteHandle:@
setRemoteHandleSelector :: Selector '[Id CXHandle] ()
setRemoteHandleSelector = mkSelector "setRemoteHandle:"

-- | @Selector@ for @localizedCallerName@
localizedCallerNameSelector :: Selector '[] (Id NSString)
localizedCallerNameSelector = mkSelector "localizedCallerName"

-- | @Selector@ for @setLocalizedCallerName:@
setLocalizedCallerNameSelector :: Selector '[Id NSString] ()
setLocalizedCallerNameSelector = mkSelector "setLocalizedCallerName:"

-- | @Selector@ for @supportsHolding@
supportsHoldingSelector :: Selector '[] Bool
supportsHoldingSelector = mkSelector "supportsHolding"

-- | @Selector@ for @setSupportsHolding:@
setSupportsHoldingSelector :: Selector '[Bool] ()
setSupportsHoldingSelector = mkSelector "setSupportsHolding:"

-- | @Selector@ for @supportsGrouping@
supportsGroupingSelector :: Selector '[] Bool
supportsGroupingSelector = mkSelector "supportsGrouping"

-- | @Selector@ for @setSupportsGrouping:@
setSupportsGroupingSelector :: Selector '[Bool] ()
setSupportsGroupingSelector = mkSelector "setSupportsGrouping:"

-- | @Selector@ for @supportsUngrouping@
supportsUngroupingSelector :: Selector '[] Bool
supportsUngroupingSelector = mkSelector "supportsUngrouping"

-- | @Selector@ for @setSupportsUngrouping:@
setSupportsUngroupingSelector :: Selector '[Bool] ()
setSupportsUngroupingSelector = mkSelector "setSupportsUngrouping:"

-- | @Selector@ for @supportsDTMF@
supportsDTMFSelector :: Selector '[] Bool
supportsDTMFSelector = mkSelector "supportsDTMF"

-- | @Selector@ for @setSupportsDTMF:@
setSupportsDTMFSelector :: Selector '[Bool] ()
setSupportsDTMFSelector = mkSelector "setSupportsDTMF:"

-- | @Selector@ for @hasVideo@
hasVideoSelector :: Selector '[] Bool
hasVideoSelector = mkSelector "hasVideo"

-- | @Selector@ for @setHasVideo:@
setHasVideoSelector :: Selector '[Bool] ()
setHasVideoSelector = mkSelector "setHasVideo:"

