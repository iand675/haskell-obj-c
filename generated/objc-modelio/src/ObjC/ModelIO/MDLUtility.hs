{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLUtility@.
module ObjC.ModelIO.MDLUtility
  ( MDLUtility
  , IsMDLUtility(..)
  , convertToUSDZ_writeToURL
  , convertToUSDZ_writeToURLSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ convertToUSDZ:writeToURL:@
convertToUSDZ_writeToURL :: (IsNSURL inputURL, IsNSURL outputURL) => inputURL -> outputURL -> IO ()
convertToUSDZ_writeToURL inputURL outputURL =
  do
    cls' <- getRequiredClass "MDLUtility"
    withObjCPtr inputURL $ \raw_inputURL ->
      withObjCPtr outputURL $ \raw_outputURL ->
        sendClassMsg cls' (mkSelector "convertToUSDZ:writeToURL:") retVoid [argPtr (castPtr raw_inputURL :: Ptr ()), argPtr (castPtr raw_outputURL :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @convertToUSDZ:writeToURL:@
convertToUSDZ_writeToURLSelector :: Selector
convertToUSDZ_writeToURLSelector = mkSelector "convertToUSDZ:writeToURL:"

