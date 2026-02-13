{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , removedOnCompletion
  , setRemovedOnCompletion
  , preferredFrameRateRange
  , setPreferredFrameRateRange
  , animationSelector
  , defaultValueForKeySelector
  , delegateSelector
  , preferredFrameRateRangeSelector
  , removedOnCompletionSelector
  , setDelegateSelector
  , setPreferredFrameRateRangeSelector
  , setRemovedOnCompletionSelector
  , setTimingFunctionSelector
  , shouldArchiveValueForKeySelector
  , timingFunctionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' animationSelector

-- | @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CAAnimation"
    sendClassMessage cls' defaultValueForKeySelector (toNSString key)

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCAAnimation caAnimation, IsNSString key) => caAnimation -> key -> IO Bool
shouldArchiveValueForKey caAnimation key =
  sendMessage caAnimation shouldArchiveValueForKeySelector (toNSString key)

-- | @- timingFunction@
timingFunction :: IsCAAnimation caAnimation => caAnimation -> IO (Id CAMediaTimingFunction)
timingFunction caAnimation =
  sendMessage caAnimation timingFunctionSelector

-- | @- setTimingFunction:@
setTimingFunction :: (IsCAAnimation caAnimation, IsCAMediaTimingFunction value) => caAnimation -> value -> IO ()
setTimingFunction caAnimation value =
  sendMessage caAnimation setTimingFunctionSelector (toCAMediaTimingFunction value)

-- | @- delegate@
delegate :: IsCAAnimation caAnimation => caAnimation -> IO RawId
delegate caAnimation =
  sendMessage caAnimation delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCAAnimation caAnimation => caAnimation -> RawId -> IO ()
setDelegate caAnimation value =
  sendMessage caAnimation setDelegateSelector value

-- | @- removedOnCompletion@
removedOnCompletion :: IsCAAnimation caAnimation => caAnimation -> IO Bool
removedOnCompletion caAnimation =
  sendMessage caAnimation removedOnCompletionSelector

-- | @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsCAAnimation caAnimation => caAnimation -> Bool -> IO ()
setRemovedOnCompletion caAnimation value =
  sendMessage caAnimation setRemovedOnCompletionSelector value

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCAAnimation caAnimation => caAnimation -> IO CAFrameRateRange
preferredFrameRateRange caAnimation =
  sendMessage caAnimation preferredFrameRateRangeSelector

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCAAnimation caAnimation => caAnimation -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caAnimation value =
  sendMessage caAnimation setPreferredFrameRateRangeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animation@
animationSelector :: Selector '[] (Id CAAnimation)
animationSelector = mkSelector "animation"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector '[Id NSString] RawId
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector '[Id NSString] Bool
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector '[] (Id CAMediaTimingFunction)
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector '[Id CAMediaTimingFunction] ()
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector '[] Bool
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector '[Bool] ()
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector '[] CAFrameRateRange
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector '[CAFrameRateRange] ()
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

