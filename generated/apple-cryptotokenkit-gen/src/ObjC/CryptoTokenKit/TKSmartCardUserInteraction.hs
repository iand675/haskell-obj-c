{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents handle to a user interaction involving the SmartCard reader.
--
-- It is a proxy object obtained as a result of invoking the userInteractionFor*** family of methods in TKSmartCardSlot and TKSmartCard.
--
-- Generated bindings for @TKSmartCardUserInteraction@.
module ObjC.CryptoTokenKit.TKSmartCardUserInteraction
  ( TKSmartCardUserInteraction
  , IsTKSmartCardUserInteraction(..)
  , runWithReply
  , cancel
  , delegate
  , setDelegate
  , initialTimeout
  , setInitialTimeout
  , interactionTimeout
  , setInteractionTimeout
  , runWithReplySelector
  , cancelSelector
  , delegateSelector
  , setDelegateSelector
  , initialTimeoutSelector
  , setInitialTimeoutSelector
  , interactionTimeoutSelector
  , setInteractionTimeoutSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Runs the interaction.
--
-- ObjC selector: @- runWithReply:@
runWithReply :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> Ptr () -> IO ()
runWithReply tkSmartCardUserInteraction  reply =
    sendMsg tkSmartCardUserInteraction (mkSelector "runWithReply:") retVoid [argPtr (castPtr reply :: Ptr ())]

-- | Attempts to cancel a running interaction. Note that for some interactions, this functionality might not be available.
--
-- Returns: Returns NO if the operation is not running, or cancelling is not supported.
--
-- ObjC selector: @- cancel@
cancel :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO Bool
cancel tkSmartCardUserInteraction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkSmartCardUserInteraction (mkSelector "cancel") retCULong []

-- | Delegate for state observing of the interaction.
--
-- ObjC selector: @- delegate@
delegate :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO RawId
delegate tkSmartCardUserInteraction  =
    fmap (RawId . castPtr) $ sendMsg tkSmartCardUserInteraction (mkSelector "delegate") (retPtr retVoid) []

-- | Delegate for state observing of the interaction.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> RawId -> IO ()
setDelegate tkSmartCardUserInteraction  value =
    sendMsg tkSmartCardUserInteraction (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Initial interaction timeout. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- initialTimeout@
initialTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO CDouble
initialTimeout tkSmartCardUserInteraction  =
    sendMsg tkSmartCardUserInteraction (mkSelector "initialTimeout") retCDouble []

-- | Initial interaction timeout. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setInitialTimeout:@
setInitialTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> CDouble -> IO ()
setInitialTimeout tkSmartCardUserInteraction  value =
    sendMsg tkSmartCardUserInteraction (mkSelector "setInitialTimeout:") retVoid [argCDouble value]

-- | Timeout after the first key stroke. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- interactionTimeout@
interactionTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO CDouble
interactionTimeout tkSmartCardUserInteraction  =
    sendMsg tkSmartCardUserInteraction (mkSelector "interactionTimeout") retCDouble []

-- | Timeout after the first key stroke. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setInteractionTimeout:@
setInteractionTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> CDouble -> IO ()
setInteractionTimeout tkSmartCardUserInteraction  value =
    sendMsg tkSmartCardUserInteraction (mkSelector "setInteractionTimeout:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWithReply:@
runWithReplySelector :: Selector
runWithReplySelector = mkSelector "runWithReply:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @initialTimeout@
initialTimeoutSelector :: Selector
initialTimeoutSelector = mkSelector "initialTimeout"

-- | @Selector@ for @setInitialTimeout:@
setInitialTimeoutSelector :: Selector
setInitialTimeoutSelector = mkSelector "setInitialTimeout:"

-- | @Selector@ for @interactionTimeout@
interactionTimeoutSelector :: Selector
interactionTimeoutSelector = mkSelector "interactionTimeout"

-- | @Selector@ for @setInteractionTimeout:@
setInteractionTimeoutSelector :: Selector
setInteractionTimeoutSelector = mkSelector "setInteractionTimeout:"

