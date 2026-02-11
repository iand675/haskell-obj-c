{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHeadElement@.
module ObjC.WebKit.DOMHTMLHeadElement
  ( DOMHTMLHeadElement
  , IsDOMHTMLHeadElement(..)
  , profile
  , setProfile
  , profileSelector
  , setProfileSelector


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

-- | @- profile@
profile :: IsDOMHTMLHeadElement domhtmlHeadElement => domhtmlHeadElement -> IO (Id NSString)
profile domhtmlHeadElement  =
  sendMsg domhtmlHeadElement (mkSelector "profile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProfile:@
setProfile :: (IsDOMHTMLHeadElement domhtmlHeadElement, IsNSString value) => domhtmlHeadElement -> value -> IO ()
setProfile domhtmlHeadElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlHeadElement (mkSelector "setProfile:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @profile@
profileSelector :: Selector
profileSelector = mkSelector "profile"

-- | @Selector@ for @setProfile:@
setProfileSelector :: Selector
setProfileSelector = mkSelector "setProfile:"

