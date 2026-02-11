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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updateAttributes:forFileAtURL:error:@
updateAttributes_forFileAtURL_error :: (IsCSImportExtension csImportExtension, IsCSSearchableItemAttributeSet attributes, IsNSURL contentURL, IsNSError error_) => csImportExtension -> attributes -> contentURL -> error_ -> IO Bool
updateAttributes_forFileAtURL_error csImportExtension  attributes contentURL error_ =
withObjCPtr attributes $ \raw_attributes ->
  withObjCPtr contentURL $ \raw_contentURL ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg csImportExtension (mkSelector "updateAttributes:forFileAtURL:error:") retCULong [argPtr (castPtr raw_attributes :: Ptr ()), argPtr (castPtr raw_contentURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateAttributes:forFileAtURL:error:@
updateAttributes_forFileAtURL_errorSelector :: Selector
updateAttributes_forFileAtURL_errorSelector = mkSelector "updateAttributes:forFileAtURL:error:"

