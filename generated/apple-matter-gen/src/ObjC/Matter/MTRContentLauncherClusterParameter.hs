{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterParameter@.
module ObjC.Matter.MTRContentLauncherClusterParameter
  ( MTRContentLauncherClusterParameter
  , IsMTRContentLauncherClusterParameter(..)
  , type_
  , setType
  , value
  , setValue
  , externalIDList
  , setExternalIDList
  , externalIDListSelector
  , setExternalIDListSelector
  , setTypeSelector
  , setValueSelector
  , typeSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSNumber)
type_ mtrContentLauncherClusterParameter =
  sendMessage mtrContentLauncherClusterParameter typeSelector

-- | @- setType:@
setType :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSNumber value) => mtrContentLauncherClusterParameter -> value -> IO ()
setType mtrContentLauncherClusterParameter value =
  sendMessage mtrContentLauncherClusterParameter setTypeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSString)
value mtrContentLauncherClusterParameter =
  sendMessage mtrContentLauncherClusterParameter valueSelector

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSString value) => mtrContentLauncherClusterParameter -> value -> IO ()
setValue mtrContentLauncherClusterParameter value =
  sendMessage mtrContentLauncherClusterParameter setValueSelector (toNSString value)

-- | @- externalIDList@
externalIDList :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSArray)
externalIDList mtrContentLauncherClusterParameter =
  sendMessage mtrContentLauncherClusterParameter externalIDListSelector

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSArray value) => mtrContentLauncherClusterParameter -> value -> IO ()
setExternalIDList mtrContentLauncherClusterParameter value =
  sendMessage mtrContentLauncherClusterParameter setExternalIDListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector '[] (Id NSArray)
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector '[Id NSArray] ()
setExternalIDListSelector = mkSelector "setExternalIDList:"

