{-# LANGUAGE DataKinds #-}
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
  , getRNGseedSelector
  , setRNGSeedToSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' setRNGSeedToSelector (toNSNumber seed)

-- | getRNGseed
--
-- gets the RNG seed value. If the value is not set it would return nil
--
-- ObjC selector: @+ getRNGseed@
getRNGseed :: IO (Id NSNumber)
getRNGseed  =
  do
    cls' <- getRequiredClass "MLCPlatform"
    sendClassMessage cls' getRNGseedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setRNGSeedTo:@
setRNGSeedToSelector :: Selector '[Id NSNumber] ()
setRNGSeedToSelector = mkSelector "setRNGSeedTo:"

-- | @Selector@ for @getRNGseed@
getRNGseedSelector :: Selector '[] (Id NSNumber)
getRNGseedSelector = mkSelector "getRNGseed"

