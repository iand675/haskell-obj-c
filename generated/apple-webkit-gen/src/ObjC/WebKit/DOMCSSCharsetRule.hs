{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSCharsetRule@.
module ObjC.WebKit.DOMCSSCharsetRule
  ( DOMCSSCharsetRule
  , IsDOMCSSCharsetRule(..)
  , encoding
  , encodingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- encoding@
encoding :: IsDOMCSSCharsetRule domcssCharsetRule => domcssCharsetRule -> IO (Id NSString)
encoding domcssCharsetRule =
  sendMessage domcssCharsetRule encodingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encoding@
encodingSelector :: Selector '[] (Id NSString)
encodingSelector = mkSelector "encoding"

