{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCPlatform
--
-- Utility class to set MLCompute global properties
--
-- Generated bindings for @MLCPlatform@.
module ObjC.MLCompute.MLCPlatform
  ( MLCPlatform
  , IsMLCPlatform(..)
  , setRNGSeedTo
  , getRNGseed
  , setRNGSeedToSelector
  , getRNGseedSelector


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

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setRNGSeedTo
--
-- sets the RNG seed. The seed should be of type long int.
--
-- ObjC selector: @+ setRNGSeedTo:@
setRNGSeedTo :: IsNSNumber seed => seed -> IO ()
setRNGSeedTo seed =
  do
    cls' <- getRequiredClass "MLCPlatform"
    withObjCPtr seed $ \raw_seed ->
      sendClassMsg cls' (mkSelector "setRNGSeedTo:") retVoid [argPtr (castPtr raw_seed :: Ptr ())]

-- | getRNGseed
--
-- gets the RNG seed value. If the value is not set it would return nil
--
-- ObjC selector: @+ getRNGseed@
getRNGseed :: IO (Id NSNumber)
getRNGseed  =
  do
    cls' <- getRequiredClass "MLCPlatform"
    sendClassMsg cls' (mkSelector "getRNGseed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setRNGSeedTo:@
setRNGSeedToSelector :: Selector
setRNGSeedToSelector = mkSelector "setRNGSeedTo:"

-- | @Selector@ for @getRNGseed@
getRNGseedSelector :: Selector
getRNGseedSelector = mkSelector "getRNGseed"

