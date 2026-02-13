{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ convertToUSDZ:writeToURL:@
convertToUSDZ_writeToURL :: (IsNSURL inputURL, IsNSURL outputURL) => inputURL -> outputURL -> IO ()
convertToUSDZ_writeToURL inputURL outputURL =
  do
    cls' <- getRequiredClass "MDLUtility"
    sendClassMessage cls' convertToUSDZ_writeToURLSelector (toNSURL inputURL) (toNSURL outputURL)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @convertToUSDZ:writeToURL:@
convertToUSDZ_writeToURLSelector :: Selector '[Id NSURL, Id NSURL] ()
convertToUSDZ_writeToURLSelector = mkSelector "convertToUSDZ:writeToURL:"

