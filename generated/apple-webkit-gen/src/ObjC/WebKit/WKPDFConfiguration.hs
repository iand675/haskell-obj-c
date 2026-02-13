{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
allowTransparentBackground wkpdfConfiguration =
  sendMessage wkpdfConfiguration allowTransparentBackgroundSelector

-- | A Boolean value indicating whether the PDF should allow transparent backgrounds.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setAllowTransparentBackground:@
setAllowTransparentBackground :: IsWKPDFConfiguration wkpdfConfiguration => wkpdfConfiguration -> Bool -> IO ()
setAllowTransparentBackground wkpdfConfiguration value =
  sendMessage wkpdfConfiguration setAllowTransparentBackgroundSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowTransparentBackground@
allowTransparentBackgroundSelector :: Selector '[] Bool
allowTransparentBackgroundSelector = mkSelector "allowTransparentBackground"

-- | @Selector@ for @setAllowTransparentBackground:@
setAllowTransparentBackgroundSelector :: Selector '[Bool] ()
setAllowTransparentBackgroundSelector = mkSelector "setAllowTransparentBackground:"

