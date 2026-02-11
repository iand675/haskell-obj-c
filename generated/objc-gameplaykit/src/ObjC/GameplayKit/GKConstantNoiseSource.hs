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
  , valueSelector
  , setValueSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ constantNoiseWithValue:@
constantNoiseWithValue :: CDouble -> IO (Id GKConstantNoiseSource)
constantNoiseWithValue value =
  do
    cls' <- getRequiredClass "GKConstantNoiseSource"
    sendClassMsg cls' (mkSelector "constantNoiseWithValue:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= retainedObject . castPtr

-- | @- initWithValue:@
initWithValue :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> CDouble -> IO (Id GKConstantNoiseSource)
initWithValue gkConstantNoiseSource  value =
  sendMsg gkConstantNoiseSource (mkSelector "initWithValue:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= ownedObject . castPtr

-- | @- value@
value :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> IO CDouble
value gkConstantNoiseSource  =
  sendMsg gkConstantNoiseSource (mkSelector "value") retCDouble []

-- | @- setValue:@
setValue :: IsGKConstantNoiseSource gkConstantNoiseSource => gkConstantNoiseSource -> CDouble -> IO ()
setValue gkConstantNoiseSource  value =
  sendMsg gkConstantNoiseSource (mkSelector "setValue:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constantNoiseWithValue:@
constantNoiseWithValueSelector :: Selector
constantNoiseWithValueSelector = mkSelector "constantNoiseWithValue:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

