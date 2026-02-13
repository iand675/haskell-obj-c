{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterDSTOffsetStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterDSTOffsetStruct
  ( MTRTimeSynchronizationClusterDSTOffsetStruct
  , IsMTRTimeSynchronizationClusterDSTOffsetStruct(..)
  , offset
  , setOffset
  , validStarting
  , setValidStarting
  , validUntil
  , setValidUntil
  , offsetSelector
  , setOffsetSelector
  , setValidStartingSelector
  , setValidUntilSelector
  , validStartingSelector
  , validUntilSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- offset@
offset :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterDSTOffsetStruct =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct offsetSelector

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setOffset mtrTimeSynchronizationClusterDSTOffsetStruct value =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct setOffsetSelector (toNSNumber value)

-- | @- validStarting@
validStarting :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
validStarting mtrTimeSynchronizationClusterDSTOffsetStruct =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct validStartingSelector

-- | @- setValidStarting:@
setValidStarting :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setValidStarting mtrTimeSynchronizationClusterDSTOffsetStruct value =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct setValidStartingSelector (toNSNumber value)

-- | @- validUntil@
validUntil :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
validUntil mtrTimeSynchronizationClusterDSTOffsetStruct =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct validUntilSelector

-- | @- setValidUntil:@
setValidUntil :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setValidUntil mtrTimeSynchronizationClusterDSTOffsetStruct value =
  sendMessage mtrTimeSynchronizationClusterDSTOffsetStruct setValidUntilSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] (Id NSNumber)
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[Id NSNumber] ()
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @validStarting@
validStartingSelector :: Selector '[] (Id NSNumber)
validStartingSelector = mkSelector "validStarting"

-- | @Selector@ for @setValidStarting:@
setValidStartingSelector :: Selector '[Id NSNumber] ()
setValidStartingSelector = mkSelector "setValidStarting:"

-- | @Selector@ for @validUntil@
validUntilSelector :: Selector '[] (Id NSNumber)
validUntilSelector = mkSelector "validUntil"

-- | @Selector@ for @setValidUntil:@
setValidUntilSelector :: Selector '[Id NSNumber] ()
setValidUntilSelector = mkSelector "setValidUntil:"

