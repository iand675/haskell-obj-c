{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together parameters for the creation of a new command queue.
--
-- Generated bindings for @MTL4CommandQueueDescriptor@.
module ObjC.Metal.MTL4CommandQueueDescriptor
  ( MTL4CommandQueueDescriptor
  , IsMTL4CommandQueueDescriptor(..)
  , label
  , setLabel
  , feedbackQueue
  , setFeedbackQueue
  , labelSelector
  , setLabelSelector
  , feedbackQueueSelector
  , setFeedbackQueueSelector


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
import ObjC.Foundation.Internal.Classes

-- | Assigns an optional label to the command queue instance for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor => mtL4CommandQueueDescriptor -> IO (Id NSString)
label mtL4CommandQueueDescriptor  =
  sendMsg mtL4CommandQueueDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional label to the command queue instance for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor, IsNSString value) => mtL4CommandQueueDescriptor -> value -> IO ()
setLabel mtL4CommandQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4CommandQueueDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns a dispatch queue to which Metal submits feedback notification blocks.
--
-- When you assign a dispatch queue via this method, Metal requires that the queue parameter you provide is a serial queue.
--
-- If you set the value of property to @nil@, the default, Metal allocates an internal dispatch queue to service feedback notifications.
--
-- ObjC selector: @- feedbackQueue@
feedbackQueue :: IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor => mtL4CommandQueueDescriptor -> IO (Id NSObject)
feedbackQueue mtL4CommandQueueDescriptor  =
  sendMsg mtL4CommandQueueDescriptor (mkSelector "feedbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns a dispatch queue to which Metal submits feedback notification blocks.
--
-- When you assign a dispatch queue via this method, Metal requires that the queue parameter you provide is a serial queue.
--
-- If you set the value of property to @nil@, the default, Metal allocates an internal dispatch queue to service feedback notifications.
--
-- ObjC selector: @- setFeedbackQueue:@
setFeedbackQueue :: (IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor, IsNSObject value) => mtL4CommandQueueDescriptor -> value -> IO ()
setFeedbackQueue mtL4CommandQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4CommandQueueDescriptor (mkSelector "setFeedbackQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @feedbackQueue@
feedbackQueueSelector :: Selector
feedbackQueueSelector = mkSelector "feedbackQueue"

-- | @Selector@ for @setFeedbackQueue:@
setFeedbackQueueSelector :: Selector
setFeedbackQueueSelector = mkSelector "setFeedbackQueue:"

