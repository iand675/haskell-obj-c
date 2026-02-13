{-# LANGUAGE DataKinds #-}
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
  , cancelSelector
  , delegateSelector
  , initialTimeoutSelector
  , interactionTimeoutSelector
  , runWithReplySelector
  , setDelegateSelector
  , setInitialTimeoutSelector
  , setInteractionTimeoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Runs the interaction.
--
-- ObjC selector: @- runWithReply:@
runWithReply :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> Ptr () -> IO ()
runWithReply tkSmartCardUserInteraction reply =
  sendMessage tkSmartCardUserInteraction runWithReplySelector reply

-- | Attempts to cancel a running interaction. Note that for some interactions, this functionality might not be available.
--
-- Returns: Returns NO if the operation is not running, or cancelling is not supported.
--
-- ObjC selector: @- cancel@
cancel :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO Bool
cancel tkSmartCardUserInteraction =
  sendMessage tkSmartCardUserInteraction cancelSelector

-- | Delegate for state observing of the interaction.
--
-- ObjC selector: @- delegate@
delegate :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO RawId
delegate tkSmartCardUserInteraction =
  sendMessage tkSmartCardUserInteraction delegateSelector

-- | Delegate for state observing of the interaction.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> RawId -> IO ()
setDelegate tkSmartCardUserInteraction value =
  sendMessage tkSmartCardUserInteraction setDelegateSelector value

-- | Initial interaction timeout. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- initialTimeout@
initialTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO CDouble
initialTimeout tkSmartCardUserInteraction =
  sendOwnedMessage tkSmartCardUserInteraction initialTimeoutSelector

-- | Initial interaction timeout. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setInitialTimeout:@
setInitialTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> CDouble -> IO ()
setInitialTimeout tkSmartCardUserInteraction value =
  sendMessage tkSmartCardUserInteraction setInitialTimeoutSelector value

-- | Timeout after the first key stroke. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- interactionTimeout@
interactionTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> IO CDouble
interactionTimeout tkSmartCardUserInteraction =
  sendMessage tkSmartCardUserInteraction interactionTimeoutSelector

-- | Timeout after the first key stroke. If set to 0, the reader-defined default timeout is used.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setInteractionTimeout:@
setInteractionTimeout :: IsTKSmartCardUserInteraction tkSmartCardUserInteraction => tkSmartCardUserInteraction -> CDouble -> IO ()
setInteractionTimeout tkSmartCardUserInteraction value =
  sendMessage tkSmartCardUserInteraction setInteractionTimeoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWithReply:@
runWithReplySelector :: Selector '[Ptr ()] ()
runWithReplySelector = mkSelector "runWithReply:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] Bool
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @initialTimeout@
initialTimeoutSelector :: Selector '[] CDouble
initialTimeoutSelector = mkSelector "initialTimeout"

-- | @Selector@ for @setInitialTimeout:@
setInitialTimeoutSelector :: Selector '[CDouble] ()
setInitialTimeoutSelector = mkSelector "setInitialTimeout:"

-- | @Selector@ for @interactionTimeout@
interactionTimeoutSelector :: Selector '[] CDouble
interactionTimeoutSelector = mkSelector "interactionTimeout"

-- | @Selector@ for @setInteractionTimeout:@
setInteractionTimeoutSelector :: Selector '[CDouble] ()
setInteractionTimeoutSelector = mkSelector "setInteractionTimeout:"

