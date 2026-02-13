{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKNavigation object can be used for tracking the loading progress of a webpage.
--
-- A navigation is returned from the web view load methods, and is also passed to the navigation delegate methods, to uniquely identify a webpage load from start to finish.
--
-- Generated bindings for @WKNavigation@.
module ObjC.WebKit.WKNavigation
  ( WKNavigation
  , IsWKNavigation(..)
  , effectiveContentMode
  , effectiveContentModeSelector

  -- * Enum types
  , WKContentMode(WKContentMode)
  , pattern WKContentModeRecommended
  , pattern WKContentModeMobile
  , pattern WKContentModeDesktop

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The content mode used when loading this webpage.
--
-- The value is either WKContentModeMobile or WKContentModeDesktop.
--
-- ObjC selector: @- effectiveContentMode@
effectiveContentMode :: IsWKNavigation wkNavigation => wkNavigation -> IO WKContentMode
effectiveContentMode wkNavigation =
  sendMessage wkNavigation effectiveContentModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effectiveContentMode@
effectiveContentModeSelector :: Selector '[] WKContentMode
effectiveContentModeSelector = mkSelector "effectiveContentMode"

