{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base animation class. *
--
-- Generated bindings for @CAAnimation@.
module ObjC.QuartzCore.CAAnimation
  ( CAAnimation
  , IsCAAnimation(..)
  , animation
  , defaultValueForKey
  , shouldArchiveValueForKey
  , timingFunction
  , setTimingFunction
  , removedOnCompletion
  , setRemovedOnCompletion
  , preferredFrameRateRange
  , setPreferredFrameRateRange
  , animationSelector
  , defaultValueForKeySelector
  , shouldArchiveValueForKeySelector
  , timingFunctionSelector
  , setTimingFunctionSelector
  , removedOnCompletionSelector
  , setRemovedOnCompletionSelector
  , preferredFrameRateRangeSelector
  , setPreferredFrameRateRangeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ animation@
animation :: IO (Id CAAnimation)
animation  =
  do
    cls' <- getRequiredClass "CAAnimation"
    sendClassMsg cls' (mkSelector "animation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CAAnimation"
    withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCAAnimation caAnimation, IsNSString key) => caAnimation -> key -> IO Bool
shouldArchiveValueForKey caAnimation  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caAnimation (mkSelector "shouldArchiveValueForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- timingFunction@
timingFunction :: IsCAAnimation caAnimation => caAnimation -> IO (Id CAMediaTimingFunction)
timingFunction caAnimation  =
  sendMsg caAnimation (mkSelector "timingFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimingFunction:@
setTimingFunction :: (IsCAAnimation caAnimation, IsCAMediaTimingFunction value) => caAnimation -> value -> IO ()
setTimingFunction caAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caAnimation (mkSelector "setTimingFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- removedOnCompletion@
removedOnCompletion :: IsCAAnimation caAnimation => caAnimation -> IO Bool
removedOnCompletion caAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caAnimation (mkSelector "removedOnCompletion") retCULong []

-- | @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsCAAnimation caAnimation => caAnimation -> Bool -> IO ()
setRemovedOnCompletion caAnimation  value =
  sendMsg caAnimation (mkSelector "setRemovedOnCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCAAnimation caAnimation => caAnimation -> IO CAFrameRateRange
preferredFrameRateRange caAnimation  =
  sendMsgStret caAnimation (mkSelector "preferredFrameRateRange") retCAFrameRateRange []

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCAAnimation caAnimation => caAnimation -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caAnimation  value =
  sendMsg caAnimation (mkSelector "setPreferredFrameRateRange:") retVoid [argCAFrameRateRange value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animation@
animationSelector :: Selector
animationSelector = mkSelector "animation"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

