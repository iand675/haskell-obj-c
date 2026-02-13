{-# LANGUAGE DataKinds #-}
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
  , feedbackQueueSelector
  , labelSelector
  , setFeedbackQueueSelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Assigns an optional label to the command queue instance for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor => mtL4CommandQueueDescriptor -> IO (Id NSString)
label mtL4CommandQueueDescriptor =
  sendMessage mtL4CommandQueueDescriptor labelSelector

-- | Assigns an optional label to the command queue instance for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor, IsNSString value) => mtL4CommandQueueDescriptor -> value -> IO ()
setLabel mtL4CommandQueueDescriptor value =
  sendMessage mtL4CommandQueueDescriptor setLabelSelector (toNSString value)

-- | Assigns a dispatch queue to which Metal submits feedback notification blocks.
--
-- When you assign a dispatch queue via this method, Metal requires that the queue parameter you provide is a serial queue.
--
-- If you set the value of property to @nil@, the default, Metal allocates an internal dispatch queue to service feedback notifications.
--
-- ObjC selector: @- feedbackQueue@
feedbackQueue :: IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor => mtL4CommandQueueDescriptor -> IO (Id NSObject)
feedbackQueue mtL4CommandQueueDescriptor =
  sendMessage mtL4CommandQueueDescriptor feedbackQueueSelector

-- | Assigns a dispatch queue to which Metal submits feedback notification blocks.
--
-- When you assign a dispatch queue via this method, Metal requires that the queue parameter you provide is a serial queue.
--
-- If you set the value of property to @nil@, the default, Metal allocates an internal dispatch queue to service feedback notifications.
--
-- ObjC selector: @- setFeedbackQueue:@
setFeedbackQueue :: (IsMTL4CommandQueueDescriptor mtL4CommandQueueDescriptor, IsNSObject value) => mtL4CommandQueueDescriptor -> value -> IO ()
setFeedbackQueue mtL4CommandQueueDescriptor value =
  sendMessage mtL4CommandQueueDescriptor setFeedbackQueueSelector (toNSObject value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @feedbackQueue@
feedbackQueueSelector :: Selector '[] (Id NSObject)
feedbackQueueSelector = mkSelector "feedbackQueue"

-- | @Selector@ for @setFeedbackQueue:@
setFeedbackQueueSelector :: Selector '[Id NSObject] ()
setFeedbackQueueSelector = mkSelector "setFeedbackQueue:"

