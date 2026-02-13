{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterLineupInfoStruct@.
module ObjC.Matter.MTRChannelClusterLineupInfoStruct
  ( MTRChannelClusterLineupInfoStruct
  , IsMTRChannelClusterLineupInfoStruct(..)
  , operatorName
  , setOperatorName
  , lineupName
  , setLineupName
  , postalCode
  , setPostalCode
  , lineupInfoType
  , setLineupInfoType
  , lineupInfoTypeSelector
  , lineupNameSelector
  , operatorNameSelector
  , postalCodeSelector
  , setLineupInfoTypeSelector
  , setLineupNameSelector
  , setOperatorNameSelector
  , setPostalCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- operatorName@
operatorName :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
operatorName mtrChannelClusterLineupInfoStruct =
  sendMessage mtrChannelClusterLineupInfoStruct operatorNameSelector

-- | @- setOperatorName:@
setOperatorName :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setOperatorName mtrChannelClusterLineupInfoStruct value =
  sendMessage mtrChannelClusterLineupInfoStruct setOperatorNameSelector (toNSString value)

-- | @- lineupName@
lineupName :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
lineupName mtrChannelClusterLineupInfoStruct =
  sendMessage mtrChannelClusterLineupInfoStruct lineupNameSelector

-- | @- setLineupName:@
setLineupName :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setLineupName mtrChannelClusterLineupInfoStruct value =
  sendMessage mtrChannelClusterLineupInfoStruct setLineupNameSelector (toNSString value)

-- | @- postalCode@
postalCode :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
postalCode mtrChannelClusterLineupInfoStruct =
  sendMessage mtrChannelClusterLineupInfoStruct postalCodeSelector

-- | @- setPostalCode:@
setPostalCode :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setPostalCode mtrChannelClusterLineupInfoStruct value =
  sendMessage mtrChannelClusterLineupInfoStruct setPostalCodeSelector (toNSString value)

-- | @- lineupInfoType@
lineupInfoType :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSNumber)
lineupInfoType mtrChannelClusterLineupInfoStruct =
  sendMessage mtrChannelClusterLineupInfoStruct lineupInfoTypeSelector

-- | @- setLineupInfoType:@
setLineupInfoType :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSNumber value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setLineupInfoType mtrChannelClusterLineupInfoStruct value =
  sendMessage mtrChannelClusterLineupInfoStruct setLineupInfoTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operatorName@
operatorNameSelector :: Selector '[] (Id NSString)
operatorNameSelector = mkSelector "operatorName"

-- | @Selector@ for @setOperatorName:@
setOperatorNameSelector :: Selector '[Id NSString] ()
setOperatorNameSelector = mkSelector "setOperatorName:"

-- | @Selector@ for @lineupName@
lineupNameSelector :: Selector '[] (Id NSString)
lineupNameSelector = mkSelector "lineupName"

-- | @Selector@ for @setLineupName:@
setLineupNameSelector :: Selector '[Id NSString] ()
setLineupNameSelector = mkSelector "setLineupName:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector '[Id NSString] ()
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @lineupInfoType@
lineupInfoTypeSelector :: Selector '[] (Id NSNumber)
lineupInfoTypeSelector = mkSelector "lineupInfoType"

-- | @Selector@ for @setLineupInfoType:@
setLineupInfoTypeSelector :: Selector '[Id NSNumber] ()
setLineupInfoTypeSelector = mkSelector "setLineupInfoType:"

