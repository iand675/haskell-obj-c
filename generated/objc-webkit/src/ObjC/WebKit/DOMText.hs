{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMText@.
module ObjC.WebKit.DOMText
  ( DOMText
  , IsDOMText(..)
  , splitText
  , replaceWholeText
  , wholeText
  , splitTextSelector
  , replaceWholeTextSelector
  , wholeTextSelector


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

-- | @- splitText:@
splitText :: IsDOMText domText => domText -> CUInt -> IO (Id DOMText)
splitText domText  offset =
  sendMsg domText (mkSelector "splitText:") (retPtr retVoid) [argCUInt (fromIntegral offset)] >>= retainedObject . castPtr

-- | @- replaceWholeText:@
replaceWholeText :: (IsDOMText domText, IsNSString content) => domText -> content -> IO (Id DOMText)
replaceWholeText domText  content =
withObjCPtr content $ \raw_content ->
    sendMsg domText (mkSelector "replaceWholeText:") (retPtr retVoid) [argPtr (castPtr raw_content :: Ptr ())] >>= retainedObject . castPtr

-- | @- wholeText@
wholeText :: IsDOMText domText => domText -> IO (Id NSString)
wholeText domText  =
  sendMsg domText (mkSelector "wholeText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @splitText:@
splitTextSelector :: Selector
splitTextSelector = mkSelector "splitText:"

-- | @Selector@ for @replaceWholeText:@
replaceWholeTextSelector :: Selector
replaceWholeTextSelector = mkSelector "replaceWholeText:"

-- | @Selector@ for @wholeText@
wholeTextSelector :: Selector
wholeTextSelector = mkSelector "wholeText"

