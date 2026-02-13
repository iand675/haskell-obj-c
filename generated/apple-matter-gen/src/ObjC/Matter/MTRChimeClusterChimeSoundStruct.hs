{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChimeClusterChimeSoundStruct@.
module ObjC.Matter.MTRChimeClusterChimeSoundStruct
  ( MTRChimeClusterChimeSoundStruct
  , IsMTRChimeClusterChimeSoundStruct(..)
  , chimeID
  , setChimeID
  , name
  , setName
  , chimeIDSelector
  , nameSelector
  , setChimeIDSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- chimeID@
chimeID :: IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct => mtrChimeClusterChimeSoundStruct -> IO (Id NSNumber)
chimeID mtrChimeClusterChimeSoundStruct =
  sendMessage mtrChimeClusterChimeSoundStruct chimeIDSelector

-- | @- setChimeID:@
setChimeID :: (IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct, IsNSNumber value) => mtrChimeClusterChimeSoundStruct -> value -> IO ()
setChimeID mtrChimeClusterChimeSoundStruct value =
  sendMessage mtrChimeClusterChimeSoundStruct setChimeIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct => mtrChimeClusterChimeSoundStruct -> IO (Id NSString)
name mtrChimeClusterChimeSoundStruct =
  sendMessage mtrChimeClusterChimeSoundStruct nameSelector

-- | @- setName:@
setName :: (IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct, IsNSString value) => mtrChimeClusterChimeSoundStruct -> value -> IO ()
setName mtrChimeClusterChimeSoundStruct value =
  sendMessage mtrChimeClusterChimeSoundStruct setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chimeID@
chimeIDSelector :: Selector '[] (Id NSNumber)
chimeIDSelector = mkSelector "chimeID"

-- | @Selector@ for @setChimeID:@
setChimeIDSelector :: Selector '[Id NSNumber] ()
setChimeIDSelector = mkSelector "setChimeID:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

