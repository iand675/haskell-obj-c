{-# LANGUAGE DataKinds #-}
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
  , byValueSelector
  , fromValueSelector
  , setByValueSelector
  , setFromValueSelector
  , setToValueSelector
  , toValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fromValue@
fromValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
fromValue caBasicAnimation =
  sendMessage caBasicAnimation fromValueSelector

-- | @- setFromValue:@
setFromValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setFromValue caBasicAnimation value =
  sendMessage caBasicAnimation setFromValueSelector value

-- | @- toValue@
toValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
toValue caBasicAnimation =
  sendMessage caBasicAnimation toValueSelector

-- | @- setToValue:@
setToValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setToValue caBasicAnimation value =
  sendMessage caBasicAnimation setToValueSelector value

-- | @- byValue@
byValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> IO RawId
byValue caBasicAnimation =
  sendMessage caBasicAnimation byValueSelector

-- | @- setByValue:@
setByValue :: IsCABasicAnimation caBasicAnimation => caBasicAnimation -> RawId -> IO ()
setByValue caBasicAnimation value =
  sendMessage caBasicAnimation setByValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fromValue@
fromValueSelector :: Selector '[] RawId
fromValueSelector = mkSelector "fromValue"

-- | @Selector@ for @setFromValue:@
setFromValueSelector :: Selector '[RawId] ()
setFromValueSelector = mkSelector "setFromValue:"

-- | @Selector@ for @toValue@
toValueSelector :: Selector '[] RawId
toValueSelector = mkSelector "toValue"

-- | @Selector@ for @setToValue:@
setToValueSelector :: Selector '[RawId] ()
setToValueSelector = mkSelector "setToValue:"

-- | @Selector@ for @byValue@
byValueSelector :: Selector '[] RawId
byValueSelector = mkSelector "byValue"

-- | @Selector@ for @setByValue:@
setByValueSelector :: Selector '[RawId] ()
setByValueSelector = mkSelector "setByValue:"

