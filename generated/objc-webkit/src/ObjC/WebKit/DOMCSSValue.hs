{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSValue@.
module ObjC.WebKit.DOMCSSValue
  ( DOMCSSValue
  , IsDOMCSSValue(..)
  , cssText
  , setCssText
  , cssValueType
  , cssTextSelector
  , setCssTextSelector
  , cssValueTypeSelector


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

-- | @- cssText@
cssText :: IsDOMCSSValue domcssValue => domcssValue -> IO (Id NSString)
cssText domcssValue  =
  sendMsg domcssValue (mkSelector "cssText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCssText:@
setCssText :: (IsDOMCSSValue domcssValue, IsNSString value) => domcssValue -> value -> IO ()
setCssText domcssValue  value =
withObjCPtr value $ \raw_value ->
    sendMsg domcssValue (mkSelector "setCssText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cssValueType@
cssValueType :: IsDOMCSSValue domcssValue => domcssValue -> IO CUShort
cssValueType domcssValue  =
  fmap fromIntegral $ sendMsg domcssValue (mkSelector "cssValueType") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cssText@
cssTextSelector :: Selector
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @cssValueType@
cssValueTypeSelector :: Selector
cssValueTypeSelector = mkSelector "cssValueType"

