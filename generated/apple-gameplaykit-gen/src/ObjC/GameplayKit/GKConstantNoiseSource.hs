{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Produces a single, constant value at all positions in the space.
--
-- Generated bindings for @GKConstantNoiseSource@.
module ObjC.GameplayKit.GKConstantNoiseSource
  ( GKConstantNoiseSource
  , IsGKConstantNoiseSource(..)
  , constantNoiseWithValue
  , initWithValue
  , value
  , setValue
  , constantNoiseWithValueSelector
  , initWithValueSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ constantNoiseWithValue:@
constantNoiseWithValue :: CDouble -> IO (Id GKConstantNoiseSource)
constantNoiseWithValue value =
  do
    cls' <- getRequiredClass "GKConstantNoiseSource"
    sendClassMessage cls' constantNoiseWithValueSelector value

-- | @- initWithValue:@
initWithValue :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> CDouble -> IO (Id GKConstantNoiseSource)
initWithValue gkConstantNoiseSource value =
  sendOwnedMessage gkConstantNoiseSource initWithValueSelector value

-- | @- value@
value :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> IO CDouble
value gkConstantNoiseSource =
  sendMessage gkConstantNoiseSource valueSelector

-- | @- setValue:@
setValue :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> CDouble -> IO ()
setValue gkConstantNoiseSource value =
  sendMessage gkConstantNoiseSource setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constantNoiseWithValue:@
constantNoiseWithValueSelector :: Selector '[CDouble] (Id GKConstantNoiseSource)
constantNoiseWithValueSelector = mkSelector "constantNoiseWithValue:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[CDouble] (Id GKConstantNoiseSource)
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CDouble
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CDouble] ()
setValueSelector = mkSelector "setValue:"

