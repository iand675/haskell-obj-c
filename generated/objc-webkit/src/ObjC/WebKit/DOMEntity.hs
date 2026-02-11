{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMEntity@.
module ObjC.WebKit.DOMEntity
  ( DOMEntity
  , IsDOMEntity(..)
  , publicId
  , systemId
  , notationName
  , publicIdSelector
  , systemIdSelector
  , notationNameSelector


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

-- | @- publicId@
publicId :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
publicId domEntity  =
  sendMsg domEntity (mkSelector "publicId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- systemId@
systemId :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
systemId domEntity  =
  sendMsg domEntity (mkSelector "systemId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notationName@
notationName :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
notationName domEntity  =
  sendMsg domEntity (mkSelector "notationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @publicId@
publicIdSelector :: Selector
publicIdSelector = mkSelector "publicId"

-- | @Selector@ for @systemId@
systemIdSelector :: Selector
systemIdSelector = mkSelector "systemId"

-- | @Selector@ for @notationName@
notationNameSelector :: Selector
notationNameSelector = mkSelector "notationName"

