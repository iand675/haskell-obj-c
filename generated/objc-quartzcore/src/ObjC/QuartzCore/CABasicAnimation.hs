{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclass for basic (single-keyframe) animations. *
--
-- Generated bindings for @CABasicAnimation@.
module ObjC.QuartzCore.CABasicAnimation
  ( CABasicAnimation
  , IsCABasicAnimation(..)
  , fromValue
  , setFromValue
  , toValue
  , setToValue
  , byValue
  , setByValue
  , fromValueSelector
  , setFromValueSelector
  , toValueSelector
  , setToValueSelector
  , byValueSelector
  , setByValueSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fromValue@
fromValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
fromValue caBasicAnimation  =
  fmap (RawId . castPtr) $ sendMsg caBasicAnimation (mkSelector "fromValue") (retPtr retVoid) []

-- | @- setFromValue:@
setFromValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setFromValue caBasicAnimation  value =
  sendMsg caBasicAnimation (mkSelector "setFromValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- toValue@
toValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
toValue caBasicAnimation  =
  fmap (RawId . castPtr) $ sendMsg caBasicAnimation (mkSelector "toValue") (retPtr retVoid) []

-- | @- setToValue:@
setToValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setToValue caBasicAnimation  value =
  sendMsg caBasicAnimation (mkSelector "setToValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- byValue@
byValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
byValue caBasicAnimation  =
  fmap (RawId . castPtr) $ sendMsg caBasicAnimation (mkSelector "byValue") (retPtr retVoid) []

-- | @- setByValue:@
setByValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setByValue caBasicAnimation  value =
  sendMsg caBasicAnimation (mkSelector "setByValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fromValue@
fromValueSelector :: Selector
fromValueSelector = mkSelector "fromValue"

-- | @Selector@ for @setFromValue:@
setFromValueSelector :: Selector
setFromValueSelector = mkSelector "setFromValue:"

-- | @Selector@ for @toValue@
toValueSelector :: Selector
toValueSelector = mkSelector "toValue"

-- | @Selector@ for @setToValue:@
setToValueSelector :: Selector
setToValueSelector = mkSelector "setToValue:"

-- | @Selector@ for @byValue@
byValueSelector :: Selector
byValueSelector = mkSelector "byValue"

-- | @Selector@ for @setByValue:@
setByValueSelector :: Selector
setByValueSelector = mkSelector "setByValue:"

