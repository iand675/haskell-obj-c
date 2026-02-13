{-# LANGUAGE DataKinds #-}
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
  , labelSelector
  , resourceViewCountSelector
  , setLabelSelector
  , setResourceViewCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Configures the number of resource views with which Metal creates the resource view pool.
--
-- ObjC selector: @- resourceViewCount@
resourceViewCount :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> IO CULong
resourceViewCount mtlResourceViewPoolDescriptor =
  sendMessage mtlResourceViewPoolDescriptor resourceViewCountSelector

-- | Configures the number of resource views with which Metal creates the resource view pool.
--
-- ObjC selector: @- setResourceViewCount:@
setResourceViewCount :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> CULong -> IO ()
setResourceViewCount mtlResourceViewPoolDescriptor value =
  sendMessage mtlResourceViewPoolDescriptor setResourceViewCountSelector value

-- | Assigns an optional label you to the resource view pool for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor => mtlResourceViewPoolDescriptor -> IO (Id NSString)
label mtlResourceViewPoolDescriptor =
  sendMessage mtlResourceViewPoolDescriptor labelSelector

-- | Assigns an optional label you to the resource view pool for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLResourceViewPoolDescriptor mtlResourceViewPoolDescriptor, IsNSString value) => mtlResourceViewPoolDescriptor -> value -> IO ()
setLabel mtlResourceViewPoolDescriptor value =
  sendMessage mtlResourceViewPoolDescriptor setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceViewCount@
resourceViewCountSelector :: Selector '[] CULong
resourceViewCountSelector = mkSelector "resourceViewCount"

-- | @Selector@ for @setResourceViewCount:@
setResourceViewCountSelector :: Selector '[CULong] ()
setResourceViewCountSelector = mkSelector "setResourceViewCount:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

