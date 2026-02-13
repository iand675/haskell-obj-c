{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterParameterStruct@.
module ObjC.Matter.MTRContentLauncherClusterParameterStruct
  ( MTRContentLauncherClusterParameterStruct
  , IsMTRContentLauncherClusterParameterStruct(..)
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
type_ :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSNumber)
type_ mtrContentLauncherClusterParameterStruct =
  sendMessage mtrContentLauncherClusterParameterStruct typeSelector

-- | @- setType:@
setType :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSNumber value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setType mtrContentLauncherClusterParameterStruct value =
  sendMessage mtrContentLauncherClusterParameterStruct setTypeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSString)
value mtrContentLauncherClusterParameterStruct =
  sendMessage mtrContentLauncherClusterParameterStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSString value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setValue mtrContentLauncherClusterParameterStruct value =
  sendMessage mtrContentLauncherClusterParameterStruct setValueSelector (toNSString value)

-- | @- externalIDList@
externalIDList :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSArray)
externalIDList mtrContentLauncherClusterParameterStruct =
  sendMessage mtrContentLauncherClusterParameterStruct externalIDListSelector

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSArray value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setExternalIDList mtrContentLauncherClusterParameterStruct value =
  sendMessage mtrContentLauncherClusterParameterStruct setExternalIDListSelector (toNSArray value)

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

