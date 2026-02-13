{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Meta-data about an endpoint of a Matter node.
--
-- Generated bindings for @MTREndpointInfo@.
module ObjC.Matter.MTREndpointInfo
  ( MTREndpointInfo
  , IsMTREndpointInfo(..)
  , init_
  , new
  , endpointID
  , deviceTypes
  , partsList
  , children
  , childrenSelector
  , deviceTypesSelector
  , endpointIDSelector
  , initSelector
  , newSelector
  , partsListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id MTREndpointInfo)
init_ mtrEndpointInfo =
  sendOwnedMessage mtrEndpointInfo initSelector

-- | @+ new@
new :: IO (Id MTREndpointInfo)
new  =
  do
    cls' <- getRequiredClass "MTREndpointInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- endpointID@
endpointID :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSNumber)
endpointID mtrEndpointInfo =
  sendMessage mtrEndpointInfo endpointIDSelector

-- | @- deviceTypes@
deviceTypes :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
deviceTypes mtrEndpointInfo =
  sendMessage mtrEndpointInfo deviceTypesSelector

-- | @- partsList@
partsList :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
partsList mtrEndpointInfo =
  sendMessage mtrEndpointInfo partsListSelector

-- | The direct children of this endpoint. This excludes indirect descendants even if they are listed in the PartsList attribute of this endpoint due to the Full-Family Pattern being used. Refer to Endpoint Composition Patterns in the Matter specification for details.
--
-- ObjC selector: @- children@
children :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
children mtrEndpointInfo =
  sendMessage mtrEndpointInfo childrenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTREndpointInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTREndpointInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector '[] (Id NSArray)
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @partsList@
partsListSelector :: Selector '[] (Id NSArray)
partsListSelector = mkSelector "partsList"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

