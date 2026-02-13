{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSImportExtension@.
module ObjC.CoreSpotlight.CSImportExtension
  ( CSImportExtension
  , IsCSImportExtension(..)
  , updateAttributes_forFileAtURL_error
  , updateAttributes_forFileAtURL_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updateAttributes:forFileAtURL:error:@
updateAttributes_forFileAtURL_error :: (IsCSImportExtension csImportExtension, IsCSSearchableItemAttributeSet attributes, IsNSURL contentURL, IsNSError error_) => csImportExtension -> attributes -> contentURL -> error_ -> IO Bool
updateAttributes_forFileAtURL_error csImportExtension attributes contentURL error_ =
  sendMessage csImportExtension updateAttributes_forFileAtURL_errorSelector (toCSSearchableItemAttributeSet attributes) (toNSURL contentURL) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateAttributes:forFileAtURL:error:@
updateAttributes_forFileAtURL_errorSelector :: Selector '[Id CSSearchableItemAttributeSet, Id NSURL, Id NSError] Bool
updateAttributes_forFileAtURL_errorSelector = mkSelector "updateAttributes:forFileAtURL:error:"

