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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- encoding@
encoding :: IsDOMCSSCharsetRule domcssCharsetRule => domcssCharsetRule -> IO (Id NSString)
encoding domcssCharsetRule  =
  sendMsg domcssCharsetRule (mkSelector "encoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encoding@
encodingSelector :: Selector
encodingSelector = mkSelector "encoding"

