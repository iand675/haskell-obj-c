{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKPDFConfiguration@.
module ObjC.WebKit.WKPDFConfiguration
  ( WKPDFConfiguration
  , IsWKPDFConfiguration(..)
  , allowTransparentBackground
  , setAllowTransparentBackground
  , allowTransparentBackgroundSelector
  , setAllowTransparentBackgroundSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value indicating whether the PDF should allow transparent backgrounds.
--
-- The default value is @NO@.
--
-- ObjC selector: @- allowTransparentBackground@
allowTransparentBackground :: IsWKPDFConfiguration wkpdfConfiguration => wkpdfConfiguration -> IO Bool
allowTransparentBackground wkpdfConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkpdfConfiguration (mkSelector "allowTransparentBackground") retCULong []

-- | A Boolean value indicating whether the PDF should allow transparent backgrounds.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setAllowTransparentBackground:@
setAllowTransparentBackground :: IsWKPDFConfiguration wkpdfConfiguration => wkpdfConfiguration -> Bool -> IO ()
setAllowTransparentBackground wkpdfConfiguration  value =
  sendMsg wkpdfConfiguration (mkSelector "setAllowTransparentBackground:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowTransparentBackground@
allowTransparentBackgroundSelector :: Selector
allowTransparentBackgroundSelector = mkSelector "allowTransparentBackground"

-- | @Selector@ for @setAllowTransparentBackground:@
setAllowTransparentBackgroundSelector :: Selector
setAllowTransparentBackgroundSelector = mkSelector "setAllowTransparentBackground:"

