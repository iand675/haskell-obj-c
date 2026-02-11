{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSPageRule@.
module ObjC.WebKit.DOMCSSPageRule
  ( DOMCSSPageRule
  , IsDOMCSSPageRule(..)
  , selectorText
  , setSelectorText
  , style
  , selectorTextSelector
  , setSelectorTextSelector
  , styleSelector


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

-- | @- selectorText@
selectorText :: IsDOMCSSPageRule domcssPageRule => domcssPageRule -> IO (Id NSString)
selectorText domcssPageRule  =
  sendMsg domcssPageRule (mkSelector "selectorText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectorText:@
setSelectorText :: (IsDOMCSSPageRule domcssPageRule, IsNSString value) => domcssPageRule -> value -> IO ()
setSelectorText domcssPageRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg domcssPageRule (mkSelector "setSelectorText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- style@
style :: IsDOMCSSPageRule domcssPageRule => domcssPageRule -> IO (Id DOMCSSStyleDeclaration)
style domcssPageRule  =
  sendMsg domcssPageRule (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectorText@
selectorTextSelector :: Selector
selectorTextSelector = mkSelector "selectorText"

-- | @Selector@ for @setSelectorText:@
setSelectorTextSelector :: Selector
setSelectorTextSelector = mkSelector "setSelectorText:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

