{-# LANGUAGE PatternSynonyms #-}
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
  , retainedReferencesSelector
  , setRetainedReferencesSelector
  , errorOptionsSelector
  , setErrorOptionsSelector
  , logStateSelector
  , setLogStateSelector

  -- * Enum types
  , MTLCommandBufferErrorOption(MTLCommandBufferErrorOption)
  , pattern MTLCommandBufferErrorOptionNone
  , pattern MTLCommandBufferErrorOptionEncoderExecutionStatus

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | retainedReferences
--
-- If YES, the created command buffer holds strong references to objects needed for it to execute. If NO, the created command buffer does not hold strong references to objects needed for it to execute.
--
-- ObjC selector: @- retainedReferences@
retainedReferences :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO Bool
retainedReferences mtlCommandBufferDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCommandBufferDescriptor (mkSelector "retainedReferences") retCULong []

-- | retainedReferences
--
-- If YES, the created command buffer holds strong references to objects needed for it to execute. If NO, the created command buffer does not hold strong references to objects needed for it to execute.
--
-- ObjC selector: @- setRetainedReferences:@
setRetainedReferences :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> Bool -> IO ()
setRetainedReferences mtlCommandBufferDescriptor  value =
    sendMsg mtlCommandBufferDescriptor (mkSelector "setRetainedReferences:") retVoid [argCULong (if value then 1 else 0)]

-- | errorOptions
--
-- A set of options to influence the error reporting of the created command buffer. See MTLCommandBufferErrorOption.
--
-- ObjC selector: @- errorOptions@
errorOptions :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO MTLCommandBufferErrorOption
errorOptions mtlCommandBufferDescriptor  =
    fmap (coerce :: CULong -> MTLCommandBufferErrorOption) $ sendMsg mtlCommandBufferDescriptor (mkSelector "errorOptions") retCULong []

-- | errorOptions
--
-- A set of options to influence the error reporting of the created command buffer. See MTLCommandBufferErrorOption.
--
-- ObjC selector: @- setErrorOptions:@
setErrorOptions :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> MTLCommandBufferErrorOption -> IO ()
setErrorOptions mtlCommandBufferDescriptor  value =
    sendMsg mtlCommandBufferDescriptor (mkSelector "setErrorOptions:") retVoid [argCULong (coerce value)]

-- | logState
--
-- Contains information related to shader logging.
--
-- ObjC selector: @- logState@
logState :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> IO RawId
logState mtlCommandBufferDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlCommandBufferDescriptor (mkSelector "logState") (retPtr retVoid) []

-- | logState
--
-- Contains information related to shader logging.
--
-- ObjC selector: @- setLogState:@
setLogState :: IsMTLCommandBufferDescriptor mtlCommandBufferDescriptor => mtlCommandBufferDescriptor -> RawId -> IO ()
setLogState mtlCommandBufferDescriptor  value =
    sendMsg mtlCommandBufferDescriptor (mkSelector "setLogState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @retainedReferences@
retainedReferencesSelector :: Selector
retainedReferencesSelector = mkSelector "retainedReferences"

-- | @Selector@ for @setRetainedReferences:@
setRetainedReferencesSelector :: Selector
setRetainedReferencesSelector = mkSelector "setRetainedReferences:"

-- | @Selector@ for @errorOptions@
errorOptionsSelector :: Selector
errorOptionsSelector = mkSelector "errorOptions"

-- | @Selector@ for @setErrorOptions:@
setErrorOptionsSelector :: Selector
setErrorOptionsSelector = mkSelector "setErrorOptions:"

-- | @Selector@ for @logState@
logStateSelector :: Selector
logStateSelector = mkSelector "logState"

-- | @Selector@ for @setLogState:@
setLogStateSelector :: Selector
setLogStateSelector = mkSelector "setLogState:"

