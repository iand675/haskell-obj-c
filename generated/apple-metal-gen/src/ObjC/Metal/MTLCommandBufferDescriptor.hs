{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLCommandBufferDescriptor
--
-- An object that you use to configure new Metal command buffer objects.
--
-- Generated bindings for @MTLCommandBufferDescriptor@.
module ObjC.Metal.MTLCommandBufferDescriptor
  ( MTLCommandBufferDescriptor
  , IsMTLCommandBufferDescriptor(..)
  , retainedReferences
  , setRetainedReferences
  , errorOptions
  , setErrorOptions
  , logState
  , setLogState
  , errorOptionsSelector
  , logStateSelector
  , retainedReferencesSelector
  , setErrorOptionsSelector
  , setLogStateSelector
  , setRetainedReferencesSelector

  -- * Enum types
  , MTLCommandBufferErrorOption(MTLCommandBufferErrorOption)
  , pattern MTLCommandBufferErrorOptionNone
  , pattern MTLCommandBufferErrorOptionEncoderExecutionStatus

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | retainedReferences
--
-- If YES, the created command buffer holds strong references to objects needed for it to execute. If NO, the created command buffer does not hold strong references to objects needed for it to execute.
--
-- ObjC selector: @- retainedReferences@
retainedReferences :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO Bool
retainedReferences mtlCommandBufferDescriptor =
  sendMessage mtlCommandBufferDescriptor retainedReferencesSelector

-- | retainedReferences
--
-- If YES, the created command buffer holds strong references to objects needed for it to execute. If NO, the created command buffer does not hold strong references to objects needed for it to execute.
--
-- ObjC selector: @- setRetainedReferences:@
setRetainedReferences :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> Bool -> IO ()
setRetainedReferences mtlCommandBufferDescriptor value =
  sendMessage mtlCommandBufferDescriptor setRetainedReferencesSelector value

-- | errorOptions
--
-- A set of options to influence the error reporting of the created command buffer. See MTLCommandBufferErrorOption.
--
-- ObjC selector: @- errorOptions@
errorOptions :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO MTLCommandBufferErrorOption
errorOptions mtlCommandBufferDescriptor =
  sendMessage mtlCommandBufferDescriptor errorOptionsSelector

-- | errorOptions
--
-- A set of options to influence the error reporting of the created command buffer. See MTLCommandBufferErrorOption.
--
-- ObjC selector: @- setErrorOptions:@
setErrorOptions :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> MTLCommandBufferErrorOption -> IO ()
setErrorOptions mtlCommandBufferDescriptor value =
  sendMessage mtlCommandBufferDescriptor setErrorOptionsSelector value

-- | logState
--
-- Contains information related to shader logging.
--
-- ObjC selector: @- logState@
logState :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO RawId
logState mtlCommandBufferDescriptor =
  sendMessage mtlCommandBufferDescriptor logStateSelector

-- | logState
--
-- Contains information related to shader logging.
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> RawId -> IO ()
setLogState mtlCommandBufferDescriptor value =
  sendMessage mtlCommandBufferDescriptor setLogStateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @retainedReferences@
retainedReferencesSelector :: Selector '[] Bool
retainedReferencesSelector = mkSelector "retainedReferences"

-- | @Selector@ for @setRetainedReferences:@
setRetainedReferencesSelector :: Selector '[Bool] ()
setRetainedReferencesSelector = mkSelector "setRetainedReferences:"

-- | @Selector@ for @errorOptions@
errorOptionsSelector :: Selector '[] MTLCommandBufferErrorOption
errorOptionsSelector = mkSelector "errorOptions"

-- | @Selector@ for @setErrorOptions:@
setErrorOptionsSelector :: Selector '[MTLCommandBufferErrorOption] ()
setErrorOptionsSelector = mkSelector "setErrorOptions:"

-- | @Selector@ for @logState@
logStateSelector :: Selector '[] RawId
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector '[RawId] ()
setLogStateSelector = mkSelector "setLogState:"

