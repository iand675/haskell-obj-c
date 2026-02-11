{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides parameters for creating a resource view pool.
--
-- Generated bindings for @MTLResourceViewPoolDescriptor@.
module ObjC.Metal.MTLResourceViewPoolDescriptor
  ( MTLResourceViewPoolDescriptor
  , IsMTLResourceViewPoolDescriptor(..)
  , resourceViewCount
  , setResourceViewCount
  , label
  , setLabel
  , resourceViewCountSelector
  , setResourceViewCountSelector
  , labelSelector
  , setLabelSelector


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

-- | Configures the number of resource views with which Metal creates the resource view pool.
--
-- ObjC selector: @- resourceViewCount@
resourceViewCount :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> IO CULong
resourceViewCount mtlResourceViewPoolDescriptor  =
  sendMsg mtlResourceViewPoolDescriptor (mkSelector "resourceViewCount") retCULong []

-- | Configures the number of resource views with which Metal creates the resource view pool.
--
-- ObjC selector: @- setResourceViewCount:@
setResourceViewCount :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> CULong -> IO ()
setResourceViewCount mtlResourceViewPoolDescriptor  value =
  sendMsg mtlResourceViewPoolDescriptor (mkSelector "setResourceViewCount:") retVoid [argCULong (fromIntegral value)]

-- | Assigns an optional label you to the resource view pool for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> IO (Id NSString)
label mtlResourceViewPoolDescriptor  =
  sendMsg mtlResourceViewPoolDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional label you to the resource view pool for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor, IsNSString value) => mtlResourceViewPoolDescriptor -> value -> IO ()
setLabel mtlResourceViewPoolDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlResourceViewPoolDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceViewCount@
resourceViewCountSelector :: Selector
resourceViewCountSelector = mkSelector "resourceViewCount"

-- | @Selector@ for @setResourceViewCount:@
setResourceViewCountSelector :: Selector
setResourceViewCountSelector = mkSelector "setResourceViewCount:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

